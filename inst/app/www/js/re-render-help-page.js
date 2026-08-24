document.addEventListener('shown.bs.tab', function() {

  const iframe = document.querySelector('iframe');

  if (
    iframe &&
    iframe.contentWindow &&
    iframe.contentWindow.MathJax &&
    iframe.contentWindow.MathJax.Hub
  ) {
    iframe.contentWindow.MathJax.Hub.Queue(
      ['Rerender', iframe.contentWindow.MathJax.Hub]
    );
  }

});
