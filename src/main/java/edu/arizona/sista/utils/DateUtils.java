package edu.arizona.sista.utils;

import java.text.*;
import java.util.*;

/**
 * Utilities for Java Date manipulation actions, such as formatting and parsing.
 *
 * NOTE: All methods in this class are static and may be used directly
 *       without instantiating this class.
 *
 *   @author Tom Hicks. 5/1/2015.
 *   Last Modified: Ported from AWCM project.
 */
public class DateUtils {

  /** The java.util.Date default string representation of the date/time format. */
  public static final String JAVA_UTIL_DATE_FORMAT_STRING = "EEE MMM dd hh:mm:ss zzz yyyy";

  /** The ISO 8601 string representation of the date/time format. */
  public static final String ISO_8601_FORMAT_STRING = "yyyy-MM-dd HH:mm:ss";

  /** The string representation of the date/time format for column titles. */
  public static final String TITLE_FORMAT_STRING = "MMM dd, yyyy";


  /** A Mountain Standard Time object. */
  public static final TimeZone MST = TimeZone.getTimeZone("GMT-7");

  /** A UTC TimeZone object. */
  public static final TimeZone UTC = TimeZone.getTimeZone("GMT+0");


  /** The date formatter for java.util.Date dates. */
  public static final SimpleDateFormat
    defaultFormatter = new SimpleDateFormat(JAVA_UTIL_DATE_FORMAT_STRING);

  static {
    defaultFormatter.setLenient(true);
    defaultFormatter.setTimeZone(MST);
  }


  /** The date formatter for ISO 8601 dates. */
  public static final SimpleDateFormat
    iso8601Formatter = new SimpleDateFormat(ISO_8601_FORMAT_STRING);

  static {
    iso8601Formatter.setLenient(false);
    iso8601Formatter.setTimeZone(UTC);
  }


  /** The date formatter for "title dates". */
  public static final SimpleDateFormat
    titleFormatter = new SimpleDateFormat(TITLE_FORMAT_STRING);

  static {
    titleFormatter.setLenient(true);
    titleFormatter.setTimeZone(MST);
  }


  /**
   * Private Constructor: all methods are static, constructor should
   * never be used.
   */
  private DateUtils () {}

  //
  // NB: the "missing" formatDefaultDate is simply java.util.Date.toString()
  //

  /**
   * Format and return the given java.util.Date as a datetime string for report titles.
   *
   * @param date the <tt>java.util.Date</tt> to be formatted into a string.
   *
   * @return the given date formatted into a format string OR an empty string
   *         if the given date was <tt>null</tt>.
   */
  public static synchronized String formatTitleDate (Date date) {
    if (date != null)
      return titleFormatter.format(date);
    return "";
  }

  /**
   * Format and return the given java.util.Date as a UTC datetime string in the
   * ISO 8601 format.
   *
   * @param date the <tt>java.util.Date</tt> to be formatted into a string.
   *
   * @return the given date formatted into a format string OR an empty string
   *         if the given date was <tt>null</tt>.
   */
  public static synchronized String formatUTC (Date date) {
    if (date != null)
      return iso8601Formatter.format(date);
    return "";
  }


  /**
   * Parse and return a java.util.Date from the given default java.util.Date datetime string.
   *
   * @param dateStr the string to be parsed into a <tt>java.util.Date</tt>.
   *
   * @return a <tt>java.util.Date</tt> parsed from the given datetime
   *         string or <tt>null</tt>, if unable to parse the given string.
   */
  public static synchronized Date parseDefaultDate (String dateStr) {
    Date dt = null;

    if (dateStr != null)
      try {
        dt = defaultFormatter.parse(dateStr);
      }
      catch (ParseException pex) { /* ignore */ }

    return dt;
  }

  /**
   * Parse and return a java.util.Date from the given "title date" datetime string.
   *
   * @param dateStr the string to be parsed into a <tt>java.util.Date</tt>.
   *
   * @return a <tt>java.util.Date</tt> parsed from the given datetime
   *         string or <tt>null</tt>, if unable to parse the given string.
   */
  public static synchronized Date parseTitleDate (String dateStr) {
    Date dt = null;

    if (dateStr != null)
      try {
        dt = titleFormatter.parse(dateStr);
      }
      catch (ParseException pex) { /* ignore */ }

    return dt;
  }


  /**
   * Parse and return a java.util.Date from the given UTC datetime string.
   * The argument string is assumed to be in the ISO 8601 format.
   *
   * @param dateStr the string to be parsed into a <tt>java.util.Date</tt>.
   *
   * @return a <tt>java.util.Date</tt> parsed from the given datetime
   *         string or <tt>null</tt>, if unable to parse the given string.
   */
  public static synchronized Date parseUTC (String dateStr) {
    Date dt = null;

    if (dateStr != null)
      try {
        dt = iso8601Formatter.parse(dateStr);
      }
      catch (ParseException pex) { /* ignore */ }

    return dt;
  }


  /**
   * Parse and return a java.util.Date from the given UTC datetime string.
   * The argument string is assumed to be in the ISO 8601 format.
   *
   * @param dateStr the string to be parsed into a <tt>java.util.Date</tt>.
   *
   * @return a <tt>java.util.Date</tt> parsed from the given datetime
   *         string or throw exception if unable to parse the given string.
   *
   * @exception IllegalArgumentException if unable to parse the given string.
   */
  public static synchronized Date requireParseUTC (String dateStr) {
    Date dt = parseUTC(dateStr);
    if (dt == null)                         // if parsing fails throw exception
      throw new IllegalArgumentException(
        "Date/Time string must have the format '" +
        ISO_8601_FORMAT_STRING + "'");

    return dt;                              // else return the date
  }

}
