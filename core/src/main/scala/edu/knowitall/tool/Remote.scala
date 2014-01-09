package edu.knowitall.tool

import dispatch.as
import dispatch.Http
import dispatch.url

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext

trait Remote {
  def urlString: String
  def timeout = 5.minutes

  val svc = url(urlString)

  def post(string: String)(implicit executor: ExecutionContext) =
    Await.result(Http(svc << string OK as.String), timeout)
}
