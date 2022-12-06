var idSite = 24;
var piwikTrackingApiUrl = 'https://agw1.bam.de/piwik/piwik.php';

var _paq = _paq || [];
_paq.push(['setTrackerUrl', piwikTrackingApiUrl]);
_paq.push(['setSiteId', idSite]);

_paq.push(["setDocumentTitle", document.domain + "/" + document.title]);
_paq.push(["setDoNotTrack", true]);
_paq.push(['trackPageView']);
_paq.push(['enableLinkTracking']);


