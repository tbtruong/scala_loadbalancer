package com.tungbtruong.loadbalancer.fast

import org.http4s.*

opaque type Urls = Vector[Uri]

object LoadBalancer {
    def build  {
        backends: Ref[IO, Urls],
        sendAndExpect: (Request[IO], Uri) => IO[String], //redirecting of HTTP requests
        addPathToBackend: (Uri, Request[IO]) => IO[Uri], //massage the HTTP request for the replica
        updateFunction: Urls => Urls, // shuffle the list of backends
        extractor: Urls => Option[Uri] //extract the backend for which I will send my new HTTP request

    }:: HttpRoutes[IO] = {
        ???
    }
}


object MyApp extends IOApp {
    override def run(args: List[String]): IO[ExitCode] = {
        val port = args(0).toInt
        val host = "localhost"
    }

    val dsl = Http4s[IO]
    import dsl.*
    val routes = HttpRoutes.of[IO] { request =>
        Ok(s"[replica:$port]You've accessed ${request.uri.path}")
    }

    val maybeServer = for {
        h <- Host.fromString(host)
        p <- Port.fromInt(port)
    } yield EmberServerBuilder
    .default[IO]
    .withHost(h)
    .withPort(p)
    .withHttpApp(routes.orNotFound)
    .build

    maybeServer.map(_.use => IO.println(s"Replica - port $port" *> IO.never))
    .getOrElse(IO.println("Host/Port combo not ok"))
    .as(ExitCode.Success)
}