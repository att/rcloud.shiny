requirejs_config_obj = {
    "baseUrl": "/lib/js",
    waitSeconds: 30,
    paths: {
        "jquery": "../../../shared.R/shiny/shared/jquery",
        "rcloud_bundle": "../../../js/rcloud_bundle",
        "mini": "../../mini",
        "selectize": "../../../shared.R/shiny/shared/selectize/js/selectize.min",
        "datatables": "../../../shared.R/shiny/shared/datatables/js/jquery.dataTables.min"
    },
    "shim": {
        datatables: {
            deps: ['jquery'],
            exports: "jQuery.fn.dataTable"
        },
        "jquery-ui": ["jquery"],
        "jquery.cookies.2.2.0": ["jquery"],
        "rserve": ["underscore"],
        "mini": ["rcloud_bundle"],
        "bootstrap": ["jquery-ui", "jquery"],
        "rcloud_bundle": ["jquery.cookies.2.2.0", "rserve"]
    }
};

var deps = [
    "bluebird", "underscore", "d3", "jquery", "jquery-ui", "bootstrap", "rserve", "mini", "rcloud_bundle", "datatables", "selectize"
];

function start_require(deps) {
    requirejs.onError = function (err) {
        if (err.requireType === 'timeout') {
            var        lines = err.toString().split('\n');
            lines = lines.slice(0, lines.length-1); // don't include link to confusing requirejs docs
            if(window.RCloud)
                RCloud.UI.fatal_dialog(["Sorry, the page timed out."].concat(lines).join('\n'), "Reload", window.location.href);
            else {
                lines.unshift('Ooops, please reload');
                var main = document.getElementById('main-div');
                main.innerHTML = '<pre>' + lines.join('\n') + '</pre>';
            }
        } else {
            throw err;
        }
    };

    window.RCloud = {};
    window.RCloud.UI = {};
    window.RCloud.UI.addons = {};
    require(deps,
        function(Promise, _, d3) {
            window.Promise = Promise;
            window._ = _;
            window.d3 = d3;
            main();
        });
}

requirejs.config(requirejs_config_obj);
start_require(deps);
