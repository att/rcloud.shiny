window.Shiny = {
    createSocket: function() {
        return window.parent.rcloud.create_fake_shiny_websocket();
    }
};
