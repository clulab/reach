Building the production image
-----------------------------
The `Dockerfile` builds a production environment for Reach that allows running
it as a web service. It uses the latest `master` branch of Reach and runs the
ApiServer on port 8080 as the default entry point. To build this image, use

```
docker build --tag reach:latest .
```

And run it as:
```
docker run -d -it -p 8080:8080 reach:latest
```
Note that `-d` runs the container in the background without blocking the
terminal it was launched from.

Building a test image
---------------------
`Dockerfile_test` builds on the `reach:latest` image, and using
build arguments, allows specifying which branch of `bioresources`,
`processors` and `reach` to check out and build. It fetches and checks out
the chosen branches, and sets build configurations to
build each system against the correct version of its dependencies. 

To build the test image, run
```
docker build -f Dockerfile_test --tag reach:test .
```
The build command takes three optional arguments: `bioresources_branch`,
`processors_branch`, and `reach_branch`. These can be specified as, for
example,
```
docker build -f Dockerfile_test --build-arg bioresources_branch=test_branch
--tag reach:test .
```

The image also sets the default entrypoint to run all REACH tests. The
container to run all the tests can be launched as follows:
```
docker run reach:test
```
