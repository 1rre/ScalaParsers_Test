import mill._, scalalib._

object Parser extends ScalaModule {
  def scalaVersion = "2.13.5"
  def ivyDeps = Agg (ivy"org.scala-lang.modules::scala-parser-combinators:1.2.0-M2")
}