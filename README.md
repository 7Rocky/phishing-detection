# Phishing detection

In this project, a dataset analysis based on URL features accessible by HTTP or HTTPS is done, using known Machine Learning algorithms.

The dataset used for the study can be found at https://www.kaggle.com/manishkc06/web-page-phishing-detection, although the original contents are at https://data.mendeley.com/datasets/c2gw7fy2j4/2, plus some Python codes to extract the features of provided URL.

The objective of the analysis is to construct several models that can be used to predict whether an URL contains some kind of phishing depending on the characteristics of that URL. The models development and analysis is done in R.

As an additional solution, it is intended to develop a tool that can detect if an URL is legitimate or contains phishing. To achieve this, firstly the URL is characterized with a Python script (based on the ones coming with the dataset); and secondly, the obtained features are entered to the models created with R, in order to predict the type of URL.

As a result, it will be possible to prevent phishing attacks by integrating this tool to browsers and email clients, to analyze an URL and warn users of a potential danger.

In this section, the usage of the Python script to characterize a list of URL is explained. The output of the script is ready to perform the predictions of the built models with R.

## Script to characterize an URL

It is intended that the program is implemented in command line interface, as shown below:

```console
$ cd scripts
$ pip install -r requirements.txt
$ python url_features.py -f url.txt -o output.csv
Results written in output.csv
Time: 33.716389179229736 seconds
```

Inside the `url.txt` file, indicated by the `-f` flag, the URL must be listed as shown in the following example:

```
https://www.google.com/
https://www.youtube.com/
https://github.com/
```

It is worth mentioning that the input URL must be accessible. It is probable that a phishing URL is only accesible during a limited period of time. Consequently, it is probable that some URL of the dataset that match phishing will not be characterized because they are not available on the Internet.

The obtained result for the accessible URL is written into a file whose name is specified with `-o` flag, CSV format (by default, the output is written to `output.csv`).

Once having the CSV file, the type of the URL could be predicted using the built models with R and the following functions:

- `predict_using_randomforest(file)`
- `predict_using_mlp(file)`
- `predict_using_C50(file)`
- `predict_using_knn(file)`
- `predict_using_rpart(file)`
- `predict_using_som(file)`

For example, the random forest model could be executed and use the corresponding prediction function from the RStudio console:

```r
> predict_using_randomforest(file = 'scripts/output.csv')
                         url       status
1    https://www.google.com/   legitimate
2   https://www.youtube.com/   legitimate
3        https://github.com/   legitimate
```

Finally, the predicted results by the model are simply obtained and clearly shown.
