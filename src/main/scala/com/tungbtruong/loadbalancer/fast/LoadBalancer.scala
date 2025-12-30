package com.tungbtruong.loadbalancer.fast

// Followed this tutorial: https://rockthejvm.com/articles/a-functional-load-balancer-with-scala-http4s-and-cats-effect#services

import org.http4s.*
import cats.syntax.all.*
import cats.effect.kernel.Ref
import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO
import org.http4s.dsl.Http4sDsl
import com.comcast.ip4s.*
import org.http4s.ember.server.EmberServerBuilder
import cats.effect.kernel.Resource
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.client.Client
import org.http4s.cleint.middleware.Logger
import pureconfig.ConfigSource

opaque type Urls = Vector[Uri]
object Urls {
    def apply(urls: Vector[Uri]): Urls = urls 

    val roundRobin: Urls => Urls = vector => 
        if (vector.isEmpty) vector
        else vector.tail :+ vector.head
    
    val first: Urls => Option[Uri] = 
        _.headOption
}
extension (urls: Vector[Uri])
    def toUrls = Urls(urls)

object LoadBalancer {
        //extract the first replica
        //forward the request
        // shuffle the replicas 
        // return the response to the user
    def apply {
        backends: Ref[IO, Urls],
        sendAndExpect: (Request[IO], Uri) => IO[String], //redirecting of HTTP requests
        addPathToBackend: (Request[IO], Uri) => IO[Uri], //massage the HTTP request for the replica
        updateFunction: Urls => Urls, // shuffle the list of backends
        extractor: Urls => Option[Uri] //extract the backend for which I will send my new HTTP request

    }:: Resources[IO, HttpRoutes[IO]] = {
        val dsl = Http4sDsl[IO]
        import dsl.*

        val routes = HttpRoutes.of[IO] { request =>
         backends.getAndUpdate(updateFunction).map(extractor).flatMap{
            _.fold(Ok("All backends are inactive")) { backendUri =>
                for {
                    uri <- addPathToBackend(backendUri, request) // Uri
                    response <- sendAndExpect(request, uri) //String
                    result <- Ok(response)
                } yield result
            }
          }
        }

        Resource.pure(routes)
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

object BigApp extends IOApp.Simple {

    def sendReq(client: Client[IO]) = (req: Request[IO], uri: Uri) =>
        client.expect[String](req.withUri(uri))

    def addReqPathToBackend(req: Request[IO, uri: Uri]): IO[Uri] =
        IO.pure {
            uri / req.uri.path.renderString.dropWhile(_ != '/') // /user/tt?id=123
        }

    def getSeedNodes: IO[Urls] = IO.fromEither(
        ConfigSource.default.at("backends").load[List[String]]
            .map(strings => strings.map(Uri.unsafeFromString))  //IO[List[Uri]]
            .map(_.toVector)
            .map(_.toUrls) // IO[Urls]
            .leftMap( e => New RuntimeException("Can't parse: " + e))
    )

    override def run: IO[Unit]  = {
        val serverResource = for {
            seedNodes <- Resource.eval(getSeedNodes)
            backends <- Resource.eval(Ref.of[IO, Urls](seedNodes))
            client <- EmberClientBuilder.default[IO].build
            loadBalancer <- LoadBalancer(
                backends,
                sendReq(client),
                addReqPathToBackend,
                Urls.roundRobin,
                Urls.first
            )

            server <- EmberServerBuilder
                .default[IO]
                .withHost(host"localhost")
                .withPort(port"8080")
                .withHttpApp(loadBalancer.orNotFound)
                .build
        } yield server

       serverResource.use(_ => IO.println("loadbalancer") *> IO.never)
    }

}