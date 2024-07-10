function getCookies(){
    var res = Cookies.get();
    Shiny.setInputValue('cookies', res);
  }
  
  $(document).on('shiny:connected', function(ev){
    getCookies();
  })
  
  Shiny.addCustomMessageHandler('cookie-set', function(msg){
    Cookies.set(msg.name, msg.value, { expires: 365});
    getCookies();
  })
  
  Shiny.addCustomMessageHandler('cookie-remove', function(msg){
    Cookies.remove(msg.name);
    getCookies();
  })
  
  
  Shiny.addCustomMessageHandler('cookie-set-secure', function(msg){
    Cookies.set(msg.name, msg.value, { expires: 365, sameSite: 'none', secure:true });
    getCookies();
  })