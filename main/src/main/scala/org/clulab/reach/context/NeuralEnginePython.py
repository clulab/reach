import torch
import torch.nn as nn
from transformers import AutoTokenizer, AutoModel

import time
import json

from py4j.clientserver import ClientServer
from py4j.java_collections import SetConverter, MapConverter, ListConverter
from py4j.java_gateway import JavaGateway

from PyScalaInterface import BioContextClassifierPyTorch

class RunPythonModel:

    data_path = "/home/zhengzhongliang/CLU_Projects/2022_ASKE/model_n_data/context_validation_data.json"
    pyscala_model_path = "/home/zhengzhongliang/CLU_Projects/2022_ASKE/model_n_data/pyscala_model_converted"

    @classmethod
    def read_json(cls, file_path):
        with open(file_path, "r") as handle:
            json_item = json.load(handle)

        return json_item

    @classmethod
    def load_and_validate(cls):
        '''
        This function loads a trained model and evaluates the model on the test split. It should reach a certain score.
        :return:
        '''

        data_json = cls.read_json(cls.data_path)

        return data_json

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
    def run_validation(cls, model):

        labels = []
        preds = []

        instances = cls.load_and_validate()

        print("model loaded! device:", model.DEVICE)

        start_time = time.time()
        for inst_idx, instance in enumerate(instances):

            pred = model.get_prediction(instance["data"])
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
    def convert_scala_input_object_to_python_format(cls, texts, eventStarts, eventEnds, contextStarts, contextEnds, evtCtxDists):
        '''
        This function converts the scala object to the python format so the script is able to deal with it.
        :param texts: list[String]
        :param eventStarts: list[Int]
        :param eventEnds: list[Int]
        :param contextStarts: list[Int]
        :param contextEnds: list[Int]
        :param evtCtxDists: list[Int]
        :return: an instance. i.e., a list of event-context pairs that is consistent with the python API
        '''

        instance = []

        for pair_idx in range(texts.size()):

            python_element = (
                texts.apply(pair_idx),  #  sentence string
                (eventStarts.apply(pair_idx), eventEnds.apply(pair_idx)),  # event indices tuple
                (contextStarts.apply(pair_idx), contextEnds.apply(pair_idx)),  # context indices tuple
                evtCtxDists.apply(pair_idx),  # 3: sentence distance, int
            )

            instance.append(python_element)

        return instance

    @classmethod
    def convert_python_int_list_to_java_int_list(cls, python_list, gateway):
        '''
        We need this function because scala could not take pure python list as input
        The method is from here: https://stackoverflow.com/questions/59951283/convert-python-list-to-java-array-using-py4j
        :return:
        '''
        # gateway = JavaGateway()
        # object_class = gateway.jvm.java.lang.Integer
        # my_java_array = gateway.new_array(object_class, len(python_list))
        # for i in range(len(python_list)):
        #     my_java_array[i]=python_list[i]

        # Second source: https://www.py4j.org/advanced_topics.html
        print("*" * 40)
        print("start processing the python to java .. ")
        java_list = gateway.jvm.java.util.ArrayList()
        for pred in python_list:
            java_list.add(pred)

        return java_list

class NeuralContextEnginePythonInterface:

    print("*" * 40)
    print("Start loading python saved neural model ...")

    model = torch.load(RunPythonModel.pyscala_model_path)
    model = model.to(model.DEVICE)
    model.eval()

    gateway = JavaGateway()  # This is used for constructing java array/list so that we can return.

    class Java:
        implements = ['org.clulab.reach.context.NeuralContextEnginePythonInterface']

    @staticmethod
    def runValidation():

        f1 = RunPythonModel.run_validation(NeuralContextEnginePythonInterface.model)

        return f1

    @staticmethod
    def forwardInstances(texts, eventStarts, eventEnds, contextStarts, contextEnds, evtCtxDists):
        '''
        The interface between scala and python. All of the arguments are passed from the scala side.
        :param texts: list[list[string]]
        :param eventStarts: list[list[int]]
        :param eventEnds: list[list[int]]
        :param contextStarts: list[list[int]]
        :param contextEnds: list[list[int]]
        :param evtCtxDists: list[list[int]]
        :return:
        '''

        preds = []
        for inst_idx in range(texts.size()):
            python_instance = RunPythonModel.convert_scala_input_object_to_python_format(
                texts.apply(inst_idx), eventStarts.apply(inst_idx), eventEnds.apply(inst_idx),
                contextStarts.apply(inst_idx), contextEnds.apply(inst_idx), evtCtxDists.apply(inst_idx)
            )

            #  print(json.dumps(python_instance, indent=2))  # This step has no problem

            pred = NeuralContextEnginePythonInterface.model.get_prediction(python_instance)
            preds.append(pred)

        # print("predictions:", preds)  # The prediction has no problem

        return RunPythonModel.convert_python_int_list_to_java_int_list(preds, gateway=NeuralContextEnginePythonInterface.gateway)

# We should a ClientServer instance, which starts a python server and a java client.
# https://www.py4j.org/py4j_client_server.html

ClientServer(java_parameters=None, python_parameters=None, python_server_entry_point=NeuralContextEnginePythonInterface)

print("Python server started! Waiting for Java request ...")

