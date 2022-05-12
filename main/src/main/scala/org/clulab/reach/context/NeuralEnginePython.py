import torch
import torch.nn as nn
from transformers import AutoTokenizer, AutoModel

import time

from py4j.java_gateway import JavaGateway
from py4j.clientserver import ClientServer

class BioContextClassifierPyTorch(nn.Module):

    '''
    This class stores the classifier and the tokenizer.
    It also has the forward and evaluation functions.
    There should be another separate class that defines the interface that interact with Scala.
    '''

    # Size of transformer's attention masks? After reading the code, it seems that this limits the length of the sentence.
    TRANSFORMER_INPUT_WIDTH = 512
    MODEL_NAME = "allenai/biomed_roberta_base"
    TOKENIZER_NAME = "allenai/biomed_roberta_base"
    ADD_SPAN_TOKENS = True

    HIDDEN_LAYER_WIDTH = 100   # This is to set the hidden layer width of the projection layer. temporarily set to 100

    # Named indexes into sentence index for readability
    CON = 0  # Context
    EVT = 1  # Event

    # Masks for non-pair context mentions and event mentions
    CON_MASK = "<CONTEXT>"
    EVT_MASK = "<EVENT>"

    # Tokens for event start/end
    EVT_START = "<EVT_START>"
    EVT_END = "<EVT_END>"
    CON_START = "<CON_START>"
    CON_END = "<CON_END>"

    DEVICE = torch.device("cuda:0")

    def __init__(self, ensemble_opt="vote"):
        '''
        Inits the model and the tokenizer
        '''

        super().__init__()
        self.tokenizer = AutoTokenizer.from_pretrained(
            self.TOKENIZER_NAME, add_prefix_space=True
        )
        new_mask_tokens = {
            "additional_special_tokens": [
                self.CON_MASK,
                self.EVT_MASK,
                self.EVT_START,
                self.EVT_END,
                self.CON_START,
                self.CON_END,
            ]
        }

        self.tokenizer.add_special_tokens(new_mask_tokens)
        self.add_span_tokens = self.ADD_SPAN_TOKENS  # This is set to true in the config file

        self.evt_start_token_id = self.tokenizer.get_added_vocab()[self.EVT_START]
        self.con_start_token_id = self.tokenizer.get_added_vocab()[self.CON_START]

        self.transformer = AutoModel.from_pretrained(self.MODEL_NAME)

        # Add six more embeddings for our <EVT> and <CON> mask tokens
        # and the <START> and <END> span markers for events and contexts
        new_size = self.transformer.config.vocab_size + 6
        self.transformer.resize_token_embeddings(new_size)

        self.projection = nn.Linear(
            self.transformer.config.hidden_size,
            self.HIDDEN_LAYER_WIDTH,
        )

        self.final_ffn = nn.Sequential(
            nn.ReLU(),
            nn.Linear(self.HIDDEN_LAYER_WIDTH, 2),
        )

        self.ensemble_opt = ensemble_opt

    def build_example(self, input_instance, debug_flag=False):
        '''
        This function should basically finish the task of the "get_item" function in the BioDataset.
        It should at least finish the following tasks:
         - identify the mention (event) and the context
         - Add the mask token properly to the input sequence
         - tokenize the sequence
         - truncate the sequence if necessary
         - Get the start indices of the mention (event) and context mask. These will be used later for the prediction.

        :param input_instance: Assuming it is a tuple: (sent, mention span)
                 - The sent is a single string, might across multiple actual sentences.
                 - The mention span is based on character not token.
        :return:
        '''

        sent_string = input_instance[0]
        evt_start_char_idx = input_instance[1][0]
        evt_end_char_idx = input_instance[1][1]
        con_start_char_idx = input_instance[2][0]
        con_end_char_idx = input_instance[2][1]

        sent_dist = input_instance[3]  # This field is not immediately useful now. But I will leave it there for now.

        time1 = time.time()

        # Step 1: add the special tokens around the event and context span in the original sentence tokens.
        if evt_start_char_idx < con_start_char_idx:
            # TODO: assert evt_e < con_s?  No overlap allowed, confirm with Enrique
            sent_string_added_span = (
                    sent_string[:evt_start_char_idx]   # End with space
                    + self.EVT_START + " "
                    + sent_string[evt_start_char_idx: evt_end_char_idx]  # No space at beginning or ending
                    + " " + self.EVT_END
                    + sent_string[evt_end_char_idx: con_start_char_idx]  # Start and end with space
                    + self.CON_START + " "
                    + sent_string[con_start_char_idx: con_end_char_idx]  # No space at beginning or ending
                    + " " + self.CON_END
                    + sent_string[con_end_char_idx:]  # Start with space
            )

        else:
            # assert con_e < evt_s  # No overlap allowed
            sent_string_added_span = (
                    sent_string[:con_start_char_idx]
                    + self.CON_START
                    + sent_string[con_start_char_idx: con_end_char_idx]
                    + self.CON_END
                    + sent_string[con_end_char_idx: evt_start_char_idx]
                    + self.EVT_START
                    + sent_string[evt_start_char_idx: evt_end_char_idx]
                    + self.EVT_END
                    + sent_string[evt_end_char_idx:]
            )

        sent_tokens_added_span = sent_string_added_span.split(" ")

        time2 = time.time()

        # Step 2: Tokenize the input sequence:

        # The input to the tokenizer does not have to be a batch. It can be just a list.
        # But here I still build it as a batch, with batch size 1. Because later the forward of model takes a batch.
        #encoded_input = self.tokenizer(sent_tokens_added_span, return_tensors="pt", truncation=False)

        encoded_input = self.tokenizer(
            sent_tokens_added_span,
            is_split_into_words=True,
            return_tensors="pt",
            truncation=False
            #padding="max_length",
            #max_length=self.TRANSFORMER_INPUT_WIDTH,
        )

        time3 = time.time()

        evt_start_idx = encoded_input["input_ids"][0].tolist().index(self.evt_start_token_id)
        con_start_idx = encoded_input["input_ids"][0].tolist().index(self.con_start_token_id)

        time4 = time.time()

        # Step 3: Do the truncation and get the new span index of the event and the context.
        if encoded_input["input_ids"].shape[1] > self.TRANSFORMER_INPUT_WIDTH:
            full_size = encoded_input["input_ids"].shape[1]
            half_width = self.TRANSFORMER_INPUT_WIDTH // 2
            quarter_width = half_width // 2

            # Judge whether the event comes first or the context comes first
            if con_start_idx < evt_start_idx:
                first = con_start_idx
                second = evt_start_idx
            else:
                first = evt_start_idx
                second = con_start_idx

            # If it's in the first window, just grab the window
            if first < half_width:
                left = encoded_input["input_ids"][0, : half_width - 1]

            # Otherwise center the window around the start span
            else:
                start = first - quarter_width
                end = first + quarter_width - 1  # -1 for <SEP> token
                left = encoded_input["input_ids"][0, start:end]
                first -= start

            # If it's in the last window, just grab the window
            if second > full_size - half_width:
                right = encoded_input["input_ids"][0, -half_width:]
                second -= full_size - (half_width * 2)

            # Otherwise center the window around the start span
            else:
                start = second - quarter_width
                end = second + quarter_width
                right = encoded_input["input_ids"][0, start:end]
                second = half_width + quarter_width

            # Update start indices for new span
            if con_start_idx < evt_start_idx:
                con_start_idx = first
                evt_start_idx = second
            else:
                evt_start_idx = first
                con_start_idx = second

            encoded_input["input_ids"] = torch.cat(
                [
                    left,
                    torch.tensor([2]),  # <SEP> token
                    right,
                ],
                dim=0,
            )

            encoded_input["input_ids"] = encoded_input["input_ids"].unsqueeze(0)
            encoded_input["attention_mask"] = encoded_input["attention_mask"][:, : self.TRANSFORMER_INPUT_WIDTH]

        time5 = time.time()

        encoded_input["input_ids"] = encoded_input["input_ids"].to(self.DEVICE)
        encoded_input["attention_mask"] = encoded_input["attention_mask"].to(self.DEVICE)
        encoded_input["start_indices"] = torch.tensor([[evt_start_idx, con_start_idx]]).to(self.DEVICE)

        if debug_flag:
            print("=" * 40)
            print("build raw input:", time2 - time1)
            print("tokenization:", time3 - time2)
            print("mask:", time4 - time3)
            print("truncation:", time5 - time4)

            input("-" * 40)

        return encoded_input

    def get_prediction(self, input_instances, debug_flag=False):
        '''

        :input_instances:
            input:
                [
                    (sent1, 1 span for mention, 1 span, for context, sentence distance),
                    (sent2, 1 span for mention, 1 span, for context, sentence distance),
                    ...
                ],

            # The sentence distance field is not used for now, but probably useful later.

            model:
                [ensemble-voting, ensemble-average]

            config:
                use the reach config to pass the parameters.

            return:
                [
                    scalar for sent 1,
                    scalar for sent 2,
                    ...
                ]

        :return:
        '''

        # This embedding all sents is to collect the all the context w.r.t. each event.
        embedding_all_sents = []
        for inst_idx, input_instance in enumerate(input_instances):

            encoded_input = self.build_example(input_instance)

            if debug_flag:
                print("=" * 40)
                print(self.DEVICE)
                print("transformer device:", next(self.transformer.parameters()).device)
                print("projection device:", next(self.projection.parameters()).device)
                print("final ffn device:", next(self.final_ffn.parameters()).device)
                print("input id device:", encoded_input["input_ids"].device)
                print("att mask device:", encoded_input["attention_mask"].device)
                print("bound device:", encoded_input["start_indices"].device)

            embedding = self.transformer(
                input_ids=encoded_input["input_ids"], attention_mask=encoded_input["attention_mask"]
            ).last_hidden_state

            # con_indices dims: batch_size * 2
            con_indices = encoded_input["start_indices"].unsqueeze(-1)
            # con_indices dims: batch_size * 2 * 1

            con_indices = con_indices.repeat(1, 1, embedding.shape[-1])
            # e.g., [[[3], [5]]] -> [[[3, 3, ... , 3], [5, 5, ..., 5]]]

            evt_indices = con_indices[:, 1, :].unsqueeze(1)
            # evt_indices: batch_size * 2 * hidden_dim -> batch_size * 1 * 768

            con_indices = con_indices[:, 0, :].unsqueeze(1)
            # con_indices: batch_size * 2 * hidden_dim -> batch_size * 1 * 768

            # TODO: I don't fully understand this API. Maybe need to verify this more carefully later.
            context_embedding = embedding.gather(1, con_indices)
            event_embedding = embedding.gather(1, evt_indices)
            # Size of each embedding: batch_size * 1 * hidden_dim

            # Maybe add this combination to a config at some point
            embedding = torch.cat([context_embedding, event_embedding], dim=1)  # size: batch_size * 2 * hidden_dim
            embedding = torch.mean(embedding, dim=1)   # size: batch_size * hidden_size

            embedding_all_sents.append(embedding)

        embedding_all_sents = torch.cat(embedding_all_sents, dim=0)
        # size: batch_size * hidden_size. Here batch size if how many contexts correspond to the event.
        # e.g., if 5 context mentions are paired with the event mention, the batch_size is 5.

        if self.ensemble_opt == "average":
            final = torch.mean(embedding_all_sents, dim=0)  # size: hidden_dim
            after_ffn = self.projection(final)  # Dim: 2
            logits = self.final_ffn(after_ffn)

            final_pred = 1 if logits[1] > logits[0] else 0

        else:  # Use the voting ensemble:
            after_ffn = self.projection(embedding_all_sents)  # Dim: batch_size * 2
            logits = self.final_ffn(after_ffn)
            preds_before_vote = torch.max(logits, dim=1)[1]

            if debug_flag:
                print(logits)
                print(preds_before_vote)
                input("---")

            final_pred = 1 if sum(preds_before_vote) > 0.5 else 0

        return final_pred

    @classmethod
    def get_p_r_f1(cls, labels, preds):

        tp = 0
        fp = 0
        fn = 0

        ep = 1e-10

        for idx in range(len(labels)):
            if preds[idx] == 1:
                if labels[idx] == 1:
                    tp += 1
                else:
                    fp += 1
            else:
                if labels[idx] == 1:
                    fn += 1

        p = tp / (tp + fp + ep)
        r = tp / (tp + fn + ep)
        f1 = 2 * p * r / (p + r + ep)

        return p, r, f1

    @classmethod
    def run_validation(cls):

        labels = []
        preds = []

        instances = cls.load_and_validate()

        print("start loading model ...")
        b = torch.load(cls.pyscala_model_path)
        b = b.to(b.DEVICE)
        b.eval()
        print("model loaded! device:", b.DEVICE)

        start_time = time.time()
        for inst_idx, instance in enumerate(instances):

            pred = b.get_prediction(instance["data"])
            answer = instance["label"]

            labels.append(answer)
            preds.append(pred)

            if (inst_idx + 1) % 500 == 0:
                p, r, f1 = cls.get_p_r_f1(labels[-500:], preds[-500:])
                print("evaluating ", inst_idx, " out of ", len(instances), pred, answer, p, r, f1, " time:", time.time() - start_time)
                start_time = time.time()

        p, r, f1 = cls.get_p_r_f1(labels, preds)
        print("final p r f1:", p, r, f1)

        return f1

    @classmethod
    def convert_scala_seq_to_python_list(cls, scala_seq):
        return [scala_seq[i] for i in range(scala_seq.size())]

    @classmethod
    def convert_scala_input_object_to_python_format(cls, scala_obj):
        '''
        This function converts the scala object to the python format so the script is able to deal with it.
        :param instance:
        :return:
        '''

        instance = {
            "data": [],
            "label": scala_obj.apply("label")
        }

        for ele_idx in range(scala_obj.apply("data").size()):
            scala_element = scala_obj.apply(ele_idx)

            python_element = (
                scala_element.apply(0),  # 0: sentence string
                cls.convert_scala_seq_to_python_list(scala_element.apply(1)),  # 1: event indices tuple
                cls.convert_scala_seq_to_python_list(scala_element.apply(2)),  # 2: context indices tuple
                scala_element.apply(3),  # 3: sentence distance, int
            )

            instance["data"].append(python_element)

        return instance

class NeuralContextEnginePythonInterface:

    class Java:
        implements = ['org.clulab.reach.context.NeuralContextEnginePythonInterface']

    @staticmethod
    def run_validation():

        f1 = BioContextClassifierPyTorch.run_validation()

        return f1

# We should a ClientServer instance, which starts a python server and a java client.
# https://www.py4j.org/py4j_client_server.html

ClientServer(java_parameters=None, python_parameters=None, python_server_entry_point=NeuralContextEnginePythonInterface)
print("Python server started! Waiting for Java request ...")