package org.clulab.reach.coserver

import scala.concurrent.duration._

import com.typesafe.config.{ Config, ConfigValueFactory, ConfigFactory }
import com.typesafe.scalalogging.LazyLogging

import akka.actor.{ ActorRef, ActorSystem, Props, Actor }

import org.clulab.processors.coserver.ProcessorCoreServerMessages._

/**
  * Reach client for the Processors Core Server.
  *   Written by: Tom Hicks. 6/9/2017.
  *   Last Modified: Initial creation.
  */
class ProcessorCoreClient (

  /** The ActorRef to the pool of actors which actually implement the calls in this trait. */
  val server: ActorRef

) extends LazyLogging {

  // fire up the actor system
  private val system = ActorSystem("proc-core-client")

  // TODO: Read configuration for RPC timeout LATER
  implicit val timeout = Timeout(5 seconds)

  /** Send the given message to the server and block until response comes back. */
  private def callServer (request: ProcessorCoreCommand): ProcessorCoreReply = {
    val response = server ? request         // call returning Future
    Await.result(response, timeout.duration).asInstanceOf[ProcessorCoreReply]
  }


  /** Constructs a document of tokens from free text; includes sentence splitting and tokenization */
  def mkDocument (text:String, keepText:Boolean = false): Document = {
    val reply = callServer(MkDocumentCmd(text, keepText))
  }

  /** Constructs a document of tokens from an array of untokenized sentences */
  def mkDocumentFromSentences (
    sentences: Iterable[String],
    keepText: Boolean = false,
    charactersBetweenSentences: Int = 1
  ): Document = {
    val reply = callServer(
      MkDocumentFromSentencesCmd(sentences, keepText, charactersBetweenSentences))
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
  }

  /**
    * Hook to allow the preprocessing of input text
    * @param origText The original input text
    * @return The preprocessed text
    */
  def preprocessText (origText:String): String = {
    val reply = callServer(PreprocessTextCmd(origText))
  }

  /** Runs preprocessText on each sentence */
  def preprocessSentences (origSentences:Iterable[String]): Iterable[String] = {
    val reply = callServer(PreprocessSentencesCmd(origSentences))
  }

  /** Runs preprocessText on each token */
  def preprocessTokens (origSentences:Iterable[Iterable[String]]): Iterable[Iterable[String]] = {
    val reply = callServer(PreprocessTokensCmd(origSentences))
  }

  //
  // The following annotators modify the document in place, which is not too elegant.
  // There are two reasons for this:
  //   (a) Some annotators (e.g., Stanford's CoreNLP) require some state
  //       (i.e., their Annotation object) to be passed between operations.
  //   (b) It is more efficient during annotate(), where all the possible operations are chained.
  //

  /** Part of speech tagging; modifies the document in place. */
  def tagPartsOfSpeech (doc:Document): Unit = {
    val reply = callServer(TagPartsOfSpeechCmd(doc))
  }

  /** Lematization; modifies the document in place. */
  def lemmatize (doc:Document): Unit = {
    val reply = callServer(LemmatizeCmd(doc))
  }

  /** NER; modifies the document in place. */
  def recognizeNamedEntities (doc:Document): Unit = {
    val reply = callServer(RecognizeNamedEntitiesCmd(doc))
  }

  /** Syntactic parsing; modifies the document in place. */
  def parse (doc:Document): Unit = {
    val reply = callServer(ParseCmd(doc))
  }

  /** Shallow parsing; modifies the document in place. */
  def chunking (doc:Document): Unit = {
    val reply = callServer(ChunkingCmd(doc))
  }

  /** SRL; modifies the document in place. */
  def labelSemanticRoles (doc:Document): Unit = {
    val reply = callServer(LabelSemanticRolesCmd(doc))
  }

  /** Coreference resolution; modifies the document in place. */
  def resolveCoreference (doc:Document): Unit = {
    val reply = callServer(ResolveCoreferenceCmd(doc))
  }

  /** Discourse parsing; modifies the document in place. */
  def discourse (doc:Document): Unit = {
    val reply = callServer(DiscourseCmd(doc))
  }

  def annotate (text:String, keepText:Boolean = false): Document = {
    val reply = callServer(AnnotateStringCmd(text, keepText))
  }

  def annotateFromSentences (sentences:Iterable[String], keepText:Boolean = false): Document = {
    val reply = callServer(AnnotateFromSentencesCmd(sentences, keepText))
  }

  def annotateFromTokens (
    sentences:Iterable[Iterable[String]],
    keepText:Boolean = false
  ): Document = {
    val reply = callServer(AnnotateFromTokensCmd(tokens, keepText))
  }

  def annotate (doc:Document): Document = {
    val reply = callServer(AnnotateCmd(doc))
  }

 }
