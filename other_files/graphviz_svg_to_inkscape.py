#!/usr/bin/python3

# EVAL REGION BEGINS HERE: |# |
# let g:My_eval_var = "silent wa | MyRunShellCmd graphviz_svg_to_inkscape.py < /home/slava/my/src.dot.svg 1>/home/slava/my/out.svg"
# EVAL REGION ENDS HERE.

from xml.dom import minidom
import sys

# read input svg file from stdin (this is output from Graphviz):
xml_root = minidom.parse(sys.stdin)

nodes = {}
edges = {}
for s in xml_root.getElementsByTagName('g'):
    # val4 = s.attributes['id'].value[:4]
    class_val = s.attributes['class'].value
    title = lambda el: el.getElementsByTagName('title')[0].firstChild.data
    node_value = lambda el: el.attributes['id'].nodeValue
    if class_val == "graph":
        graph = s
    elif class_val == "node":
        nodes[title(s)] = node_value(s)
    elif class_val == "edge":
        edges[node_value(s)] = title(s).split("->")
        # Remove child "polygon", which is the GraphViz arrow marker.
        for thing in s.childNodes:
            if thing.nodeType == s.ELEMENT_NODE and thing.tagName == "polygon":
                s.removeChild(thing)
                break

# To each edge path, add an Inkscape arrow marker in the attributes.
# For each edge move edge path to graph level and delete the edge.
for s in xml_root.getElementsByTagName('g'):
    if s.attributes['class'].value == "edge":
        for thing in s.childNodes:
            if thing.nodeType == s.ELEMENT_NODE and thing.tagName == "path":
                thing.setAttribute("inkscape:connector-type", "polyline")
                thing.setAttribute("inkscape:connector-curvature", "3")
                edge_id = edges[s.attributes['id'].value]
                thing.setAttribute("inkscape:connection-start", "#" + nodes[edge_id[0]])
                thing.setAttribute("inkscape:connection-end", "#" + nodes[edge_id[1]])
                graph.appendChild(thing)
        graph.removeChild(s)

for s in xml_root.getElementsByTagName('svg'):
    s.setAttribute("xmlns:inkscape", "http://www.inkscape.org/namespaces/inkscape")

xml_root.writexml(sys.stdout)
