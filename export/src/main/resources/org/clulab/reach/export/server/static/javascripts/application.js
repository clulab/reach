if (typeof jQuery !== 'undefined') {

  (function($) {

    $('#spinner').ajaxStart(function() {
      $(this).fadeIn();
    }).ajaxStop(function() {
      $(this).fadeOut();
    });

  })(jQuery);

}


$(document).ready(function() {

  $('#about-toggle').click(function(ev) {
    $('#about-panel').toggle();
    if ($('#about-panel').is(':hidden'))
      $('#about-toggle').text('Show');
    else
      $('#about-toggle').text('Hide');
  });
  $('#about-panel').show();

});
