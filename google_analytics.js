// Google Analytics JS
// Loaded dynamically using variable substitution for the
// Analytics gtag ID (GA4)

// Import the Google Tag JS and add to the html body
var js = document.createElement("script");
js.type = "text/javascript";
js.src = "https://www.googletagmanager.com/gtag/js?id=<<GOOGLE_ANALYTICS_ID>>";
document.body.appendChild(js);

// Setup gtag functions (to push data to Analytics)
window.dataLayer = window.dataLayer || [];
function gtag(){dataLayer.push(arguments);}
gtag('js', new Date());
gtag('config', '<<GOOGLE_ANALYTICS_ID>>');

// add an add-event message to allow events to be passed
// to gtag (Analytics)
Shiny.addCustomMessageHandler('add-event', function(msg){
    gtag('event', msg.value, {
        'event_value': msg.value
    });
})