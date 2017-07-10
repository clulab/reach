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
  *   Last Modified: Reset one leftover debug statement.
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

  //
  // The following annotators modify the document in place, which is not too elegant.
  // There are two reasons for this:
  //   (a) Some annotators (e.g., Stanford's CoreNLP) require some state
  //       (i.e., their Annotation object) to be passed between operations.
  //   (b) It is more efficient during annotate(), where all the possible operations are chained.
  //

  /** Part of speech tagging; modifies the document in place. */
  // def tagPartsOfSpeech (doc:Document): Unit = {
  //   val reply = callServer(TagPartsOfSpeechCmd(doc))
  // }

  // /** Lematization; modifies the document in place. */
  // def lemmatize (doc:Document): Unit = {
  //   val reply = callServer(LemmatizeCmd(doc))
  // }

  // /** NER; modifies the document in place. */
  // def recognizeNamedEntities (doc:Document): Unit = {
  //   val reply = callServer(RecognizeNamedEntitiesCmd(doc))
  // }

  // /** Syntactic parsing; modifies the document in place. */
  // def parse (doc:Document): Unit = {
  //   val reply = callServer(ParseCmd(doc))
  // }

  // /** Shallow parsing; modifies the document in place. */
  // def chunking (doc:Document): Unit = {
  //   val reply = callServer(ChunkingCmd(doc))
  // }

  // /** SRL; modifies the document in place. */
  // def labelSemanticRoles (doc:Document): Unit = {
  //   val reply = callServer(LabelSemanticRolesCmd(doc))
  // }

  // /** Coreference resolution; modifies the document in place. */
  // def resolveCoreference (doc:Document): Unit = {
  //   val reply = callServer(ResolveCoreferenceCmd(doc))
  // }

  // /** Discourse parsing; modifies the document in place. */
  // def discourse (doc:Document): Unit = {
  //   val reply = callServer(DiscourseCmd(doc))
  // }

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
