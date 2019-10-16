
var gsn = (function() {
    var build = function( canvasId, detailsId, graph, details, dependencies, defaultHighlighting) {

        var canvas = document.getElementById( canvasId);

        var options = {
          height:"100%"
        , width:"100%"
        , layout: {hierarchical: {sortMethod: "directed", nodeSpacing: 250, treeSpacing: 400, levelSeparation: 200, parentCentralization: true}}
        , edges: {arrows: {to: {enabled:true}}}
        , nodes: {widthConstraint:{maximum: 200}}
        , physics: false
        // , physics:{barnesHut:{"avoidOverlap": 1.2, "springConstant": 0}}
        }; // , physics:{barnesHut:{gravitationalConstant:-4000}}};
        var nodes = new vis.DataSet( graph.nodes);
        var edges = new vis.DataSet( graph.edges);
        var network = new vis.Network(canvas, {nodes:nodes, edges:edges}, options);

        network.on("deselectNode", function(params) {
            // Reset details.
            $('#'+detailsId).html($("<p>No node selected.</p>")); // Unfortunately need to copy this over.

            // Reset node highlighting.
            if (defaultHighlighting) {
                nodes.update( defaultHighlighting);
            }
        });

        network.on("selectNode", function(params) {
            // Get node id.
            var nodeId = params.nodes[0];

            // Load details for element.
            if ( details) {
                var detail = details[nodeId];
                if ( detail) {
                    $('#'+detailsId).html(detail);
                }
            }

            // Highlight dependency nodes.
            if ( dependencies) {
                var dependency = dependencies[nodeId];
                if ( dependency) {
                    nodes.update( dependency);
                }
            }
        });

    };
    
    var module = {
        build : build
    };

    return module;
})();
