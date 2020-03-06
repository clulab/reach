Building the base image
-----------------------
The `Dockerfile` installs all necessary dependencies and compiles the latest
master of `bioresources`, `processors`, and `reach` from source. To build the
image, run

```
docker build --tag reach:latest .
```

Building a test image
---------------------
The `Dockerfile_test` builds on the `reach:latest` base image, and using
build arguments, allows specifying which branch of `bioresources`,
`processors` and `reach` to check out and build. It fetches and checks out
the chosen branches, and sets build configurations to
build each system against the correct version of its dependencies. The
rationale behind separating the two Dockerfiles is that the base image can
take a long time to build but each test build is typically much faster.

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

The image also defines a default entrypoint for running all REACH tests as
follows:
```
docker run reach:test
```
