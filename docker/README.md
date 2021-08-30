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
