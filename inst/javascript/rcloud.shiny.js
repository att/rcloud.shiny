((function() {

var sockets_ = [];
var ocaps_ = null;
var title_ = "RCloud", favicon_;
var SHINY_HTML_LOCATION = 'shared.R/rcloud.shiny/shiny.html';
var debugEnabled = false;
var initialized = false;

function debug(msg, arg) {
  if(debugEnabled) {
    console.debug(msg, arg);
  }
}

function fakeWebSocket() {
    var fws = {
        readyState: false,
        send: function(msg) {
            debug("client to Shiny: ", JSON.parse(msg));
            ocaps_.sendAsync(id, msg).then(function(response) {
                debug("Shiny response: ", response);
            });
        }
    };
    var id = sockets_.length;
    sockets_.push(fws);
    fws.id = id;
    ocaps_.connectAsync(id).then(function() {
        fws.readyState = true;
        fws.onopen();
    });
    return fws;
}

function isMini() {
  return window.document.location.pathname.endsWith(SHINY_HTML_LOCATION);
}

return {
    init: function(ocaps, k) {
         if(isMini()) {
            ocaps_ = RCloud.promisify_paths(ocaps, [["connect"], ["send"]]);
            if(!initialized) {
                  window.rcloud.create_fake_shiny_websocket = function() {
                      return fakeWebSocket();
                  };
          
                  window.setInterval(function() {
                      $('iframe.rcloud-shiny').each(function() {
                          var shtitle = this.contentWindow.document.title;
                          if(shtitle !== title_)
                              document.title = title_ = shtitle;
                          var favlink = this.contentWindow.document.querySelector("link[rel*='icon']");
                          if(!favicon_ && favlink) {
                              // adapted from http://stackoverflow.com/questions/260857/changing-website-favicon-dynamically#260876
                              var link = document.querySelector("link[rel*='icon']") || document.createElement('link');
                              link.type = 'image/x-icon';
                             link.rel = favlink.rel;
                             link.href = favlink.href;
                             document.getElementsByTagName('head')[0].appendChild(link);
                             favicon_ = link;
                        }
                     });
                 }, 2000);
             }
         } else {
           RCloud.UI.share_button.add({ 
                   'shiny.html': {
                        sort: 4000,
                        page: SHINY_HTML_LOCATION
                    }
          });
        }
        initialized = true;
        k({hash: window.location.hash, search: window.location.search});
    },
    on_message: function(id, msg, k) {
        if(_.isArray(msg)) {
            // looks like shiny switched json libraries and now they're sending objects
            // instead of pseudo-scalars
            if(msg.length > 1) console.log('rcloud.shiny: whoops, more than one element?');
            msg = msg[0];
        }
        var msj = JSON.parse(msg);
        debug("Shiny to client: ", msj);
        if(msj && msj.values && msj.values.mytable1 && msj.values.mytable1.x && msj.values.mytable1.x.options)
            debug("DT options: ", msj.values.mytable1.x.options);
        // [id] ?
        sockets_[0].onmessage({data:msg});
        k();
    }
};
})()) /*jshint -W033 */ // this is an expression not a statement
