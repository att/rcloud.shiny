

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
  return RCloud.UI.advanced_menu.add === null || RCloud.UI.advanced_menu.add === undefined;
}

function getDocHeight(Di) {
    var D = Di[0];

    if (Di.find('body').css('overflow') === 'hidden') {
        return Math.max(
            D.documentElement.offsetHeight,
            D.documentElement.clientHeight
        );

    } else {
        return Math.max(
            Math.max(D.body.scrollHeight, D.documentElement.scrollHeight),
            Math.max(D.body.offsetHeight, D.documentElement.offsetHeight),
            Math.max(D.body.clientHeight, D.documentElement.clientHeight)
        );
    }
}

var lastWidths = { };

function size_this(div, reset) {
    // Check if the widget has a <body> already. If not, we need to wait
    // a bit

    var Di = $(div).find('iframe').contents();
    var D = Di[0];

    if (!div.id) {
        // Do nothing if the div is not there at all

    } else if (!D || !D.body || !Di.is('visible')) {
        setTimeout(function() { size_this(div, reset); }, 100);

    } else {
        // Check if the width of the iframe is different. If not, then
        // we don't need to do anything.
        var rcid = div.id;
        var width = $(div).find('iframe').width();
        if (reset || !(rcid in lastWidths) || (lastWidths[rcid] != width)) {
            var h = getDocHeight(Di);
            $(div).find('iframe').height(h);
            $(div).find('iframe').attr('height', h);
        }
        lastWidths[rcid] = width;
    }
}

function resize_all(reset) {
    var widgets = $('.rcloud-shinyapp-content');
    $.map(
        widgets,
        function(w) {
            setTimeout(function() { size_this(w, reset) }, 200);
        }
    );

    return widgets.length;
}

var hooks = false;

function add_hooks() {
    if (!hooks) {
        hooks = true;
        window.addEventListener('resize', resize_all, true);
    }
}

// The resizer is mainly for mini.html, but might be handy for
// notebook as well, if some widgets resize very slowly.

var lastWidth = window.innerWidth;

$(document).ready(function() {
    add_hooks();
    function resizer(reset) {
        var num_widgets = resize_all(reset);
        var interval = 200;
        if (num_widgets > 0) {
            setTimeout(resizer, 5000);
        } else {
            setTimeout(function() { resizer(true) }, interval);
        }
    }
    resizer(lastWidth < window.innerWidth);
    lastWidth = window.innerWidth;
});

function initWidget(div, html, k) {
    Promise.resolve(undefined)
            .then(function() {
                $(div).html(html);
                setTimeout(function() { size_this($(div), true); }, 100);
            });
    k(null, div);
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
            ocaps_ = RCloud.promisify_paths(ocaps, [["connect"], ["send"]]);
            if(!initialized) {
                  window.rcloud.create_fake_shiny_websocket = function() {
                      return fakeWebSocket();
                  };
                 RCloud.UI.share_button.add({ 
                         'shiny.html': {
                              sort: 4000,
                              page: SHINY_HTML_LOCATION
                          }
                });
            }
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
    },
    create: function(div, html, k) {
        initWidget(div, html, k);
    },

    is_mini: function(k) {
      k(null, isMini());
    } 
    
};
})()) /*jshint -W033 */ // this is an expression not a statement
