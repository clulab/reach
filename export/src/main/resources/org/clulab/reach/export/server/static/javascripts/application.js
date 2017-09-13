var ContentLoader = (function($) {

  $('#spinner').ajaxStart(function() {
    $(this).fadeIn();
  }).ajaxStop(function() {
    $(this).fadeOut();
  });

  function contentSubmitted () {
    $('#spinner').fadeIn();
  }

  // Public API
  //
  return {
    contentSubmitted: contentSubmitted
  };

}) (jQuery);


$(document).ready(function() {

  $('#spinner').hide();

  $('#about-toggle').click(function(ev) {
    $('#about-panel').toggle();
    if ($('#about-panel').is(':hidden'))
      $('#about-toggle').text('Show');
    else
      $('#about-toggle').text('Hide');
  });
  $('#about-panel').show();

});
