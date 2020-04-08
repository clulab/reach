## Training

THE `SVMContextEngine` requires a model previously trained. To train the model from scratch the following files are required

To run this script, you will need the training dataset found at `main/src/main/resources/org/clulab/context/svmFeatures/grouped_features.csv.gz` expected to be a gziped csv file.
Also, a list of the features to be used for training, found as a comma-separated string in `main/src/main/resources/org/clulab/context/svmFeatures/specific_features.txt`

To train the model from scratch use the following script:

```
sbt 'runMain org.clulab.reach.context.svm_scripts.TrainSVMContextClassifier main/src/main/resources/org/clulab/context/svmFeatures/grouped_features.csv.gz main/src/main/resources/org/clulab/context/svmFeatures/svm_model.dat main/src/main/resources/org/clulab/context/svmFeatures/specific_features.txt'
```

## Usage Configuration

The context engine is governed by the `svmContextEngine` section of `application.conf`. To indicate REACH to use the SVMContextEngine, specify `SVMPolicy` type and specify the size of the sentence window in the `bound` parameter:

```
contextEngine{
  type = SVMPolicy
  params = {

    bound = 3
  }
}
```
The trained model *must* exist in the file `main/src/main/resources/org/clulab/context/svmFeatures/svm_model.dat`. This path is hardcoded and is not a parameter.
