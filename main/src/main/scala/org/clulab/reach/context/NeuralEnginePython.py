import torch
import torch.nn as nn
from transformers import AutoTokenizer, AutoModel

import time
import json

from py4j.clientserver import ClientServer
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

        instance = []

        for ele_idx in range(scala_obj.size()):
            scala_element = scala_obj.apply(ele_idx)

            python_element = (
                scala_element.apply(0),  # 0: sentence string
                cls.convert_scala_seq_to_python_list(scala_element.apply(1)),  # 1: event indices tuple
                cls.convert_scala_seq_to_python_list(scala_element.apply(2)),  # 2: context indices tuple
                scala_element.apply(3),  # 3: sentence distance, int
            )

            instance.append(python_element)

        return instance

class NeuralContextEnginePythonInterface:

    print("Start loading python saved neural model ...")

    b = torch.load(RunPythonModel.pyscala_model_path)
    b = b.to(b.DEVICE)
    b.eval()

    class Java:
        implements = ['org.clulab.reach.context.NeuralContextEnginePythonInterface']

    @staticmethod
    def runValidation():

        f1 = RunPythonModel.run_validation()

        return f1

    @staticmethod
    def forwardOneInstance(scala_instance):

        python_instance = RunPythonModel.convert_scala_input_object_to_python_format(scala_instance)

        print(json.dumps(python_instance, indents=2))

        pred = NeuralContextEnginePythonInterface.b.get_prediction(python_instance)

        return pred

# We should a ClientServer instance, which starts a python server and a java client.
# https://www.py4j.org/py4j_client_server.html

ClientServer(java_parameters=None, python_parameters=None, python_server_entry_point=NeuralContextEnginePythonInterface)

print("Python server started! Waiting for Java request ...")

