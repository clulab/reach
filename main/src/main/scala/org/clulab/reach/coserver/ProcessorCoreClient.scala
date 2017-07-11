package org.clulab.reach.coserver

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.language.postfixOps

import com.typesafe.config.{ Config, ConfigValueFactory, ConfigFactory }
import com.typesafe.scalalogging.LazyLogging

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import org.clulab.processors._
import org.clulab.processors.coserver.ProcessorCoreServer
import org.clulab.processors.coserver.ProcessorCoreServerMessages._

/**
  * Reach client for the Processors Core Server.
  *   Written by: Tom Hicks. 6/9/2017.
  *   Last Modified: Restore annotator call tests.
  */
class ProcessorCoreClient extends LazyLogging {

  // fire up the actor system
  private val system = ActorSystem("proc-core-server")

  // load application configuration from the configuration file
  private val config = ConfigFactory.load().getConfig("ProcessorCoreClient")

  // figure out a good timeout value for requests to the server
  private val patience = if (config.hasPath("askTimeout")) config.getInt("askTimeout") else 30
  implicit val timeout = Timeout(patience seconds)

  // fire up the processor core server and get a ref to the message router
  val router: ActorRef = ProcessorCoreServer.router
  logger.debug(s"(ProcessorCoreClient): router: ${router}")

  /** Send the given message to the server and block until response comes back. */
  private def callServer (request: ProcessorCoreCommand): ProcessorCoreReply = {
    val response = router ? request         // call returning Future
    Await.result(response, timeout.duration).asInstanceOf[ProcessorCoreReply]
  }


  /** Constructs a document of tokens from free text; includes sentence splitting and tokenization */
  def mkDocument (text:String, keepText:Boolean = false): Document = {
    val reply = callServer(MkDocumentCmd(text, keepText))
    reply.asInstanceOf[DocumentMsg].doc
  }

  /** Constructs a document of tokens from an array of untokenized sentences */
  def mkDocumentFromSentences (
    sentences: Iterable[String],
    keepText: Boolean = false,
    charactersBetweenSentences: Int = 1
  ): Document = {
    val reply = callServer(
      MkDocumentFromSentencesCmd(sentences, keepText, charactersBetweenSentences))
    reply.asInstanceOf[DocumentMsg].doc
  }

  /** Constructs a document of tokens from an array of tokenized sentences */
  def mkDocumentFromTokens (
    sentences: Iterable[Iterable[String]],
    keepText: Boolean = false,
    charactersBetweenSentences: Int = 1,
    charactersBetweenTokens: Int = 1
  ): Document = {
    val reply = callServer(
      MkDocumentFromTokensCmd(sentences, keepText,
                              charactersBetweenSentences, charactersBetweenTokens))
    reply.asInstanceOf[DocumentMsg].doc
  }

  /**
    * Hook to allow the preprocessing of input text
    * @param origText The original input text
    * @return The preprocessed text
    */
  def preprocessText (origText:String): String = {
    val reply = callServer(PreprocessTextCmd(origText))
    reply.asInstanceOf[TextMsg].text
  }

  /** Runs preprocessText on each sentence */
  def preprocessSentences (origSentences:Iterable[String]): Iterable[String] = {
    val reply = callServer(PreprocessSentencesCmd(origSentences))
    reply.asInstanceOf[SentencesMsg].sentences
  }

  /** Runs preprocessText on each token */
  def preprocessTokens (origSentences:Iterable[Iterable[String]]): Iterable[Iterable[String]] = {
    val reply = callServer(PreprocessTokensCmd(origSentences))
    reply.asInstanceOf[TokensMsg].tokens
  }


  /** Part of speech tagging. Modified document is returned. */
  def tagPartsOfSpeech (doc:Document): Document = {
    val reply = callServer(TagPartsOfSpeechCmd(doc))
    reply.asInstanceOf[DocumentMsg].doc
  }

  /** Lematization. Modified document is returned. */
  def lemmatize (doc:Document): Unit = {
    val reply = callServer(LemmatizeCmd(doc))
  }

  /** NER - Named Entity Recognition. Modified document is returned. */
  def recognizeNamedEntities (doc:Document): Unit = {
    val reply = callServer(RecognizeNamedEntitiesCmd(doc))
  }

  /** Syntactic parsing. Modified document is returned. */
  def parse (doc:Document): Unit = {
    val reply = callServer(ParseCmd(doc))
  }

  /** Shallow parsing. Modified document is returned. */
  def chunking (doc:Document): Unit = {
    val reply = callServer(ChunkingCmd(doc))
  }

  /** SRL - Semantic Role Labeling. Modified document is returned. */
  def labelSemanticRoles (doc:Document): Unit = {
    val reply = callServer(LabelSemanticRolesCmd(doc))
  }

  /** Coreference resolution. Modified document is returned. */
  def resolveCoreference (doc:Document): Unit = {
    val reply = callServer(ResolveCoreferenceCmd(doc))
  }

  /** Discourse parsing. Modified document is returned. */
  def discourse (doc:Document): Unit = {
    val reply = callServer(DiscourseCmd(doc))
  }

  def annotate (text:String, keepText:Boolean = false): Document = {
    val reply = callServer(AnnotateTextCmd(text, keepText))
    reply.asInstanceOf[DocumentMsg].doc
  }

  def annotateFromSentences (sentences:Iterable[String], keepText:Boolean = false): Document = {
    val reply = callServer(AnnotateFromSentencesCmd(sentences, keepText))
    reply.asInstanceOf[DocumentMsg].doc
  }

  def annotateFromTokens (
    sentences:Iterable[Iterable[String]],
    keepText:Boolean = false
  ): Document = {
    val reply = callServer(AnnotateFromTokensCmd(sentences, keepText))
    reply.asInstanceOf[DocumentMsg].doc
  }

  def annotate (doc:Document): Document = {
    val reply = callServer(AnnotateCmd(doc))
    reply.asInstanceOf[DocumentMsg].doc
  }

 }
