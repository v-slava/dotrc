#!/usr/bin/python3

from xml.dom import minidom
import sys

# read input svg file from stdin (this is output from Graphviz):
graphvizSVGFile = minidom.parse(sys.stdin)

# 2. For each node, get id and title.
nodeTitles = {}
for s in graphvizSVGFile.getElementsByTagName('g'):
    if s.attributes['id'].value[:4] == "node":
        nodeTitles[s.getElementsByTagName('title')[0].firstChild.data] = s.attributes['id'].nodeValue

# 3. For each arc, parse the title, and match the corresponding ids of the nodes, add the ids to the arc,
edgeTitles = {}
for s in graphvizSVGFile.getElementsByTagName('g'):
    if s.attributes['id'].value[:4] == "edge":
        edgeTitle = s.getElementsByTagName('title')[0].firstChild.data
        edgeTitles [s.attributes['id'].nodeValue] = edgeTitle.split("->")

# 4. Add connector elements to Graphviz.svg.
# For each arc, delete the GraphViz arrow marker,
for s in graphvizSVGFile.getElementsByTagName('g'):
    if s.attributes['id'].value[:4] == "edge":
        # Remove child "polygon", which is the GraphViz arrow marker.
        for thing in s.childNodes:
            if thing.nodeType == s.ELEMENT_NODE and thing.tagName == "polygon":
                s.removeChild(thing)
                break

# 5. To each edge path, add an Inkscape arrow marker in the attributes.
for s in graphvizSVGFile.getElementsByTagName('g'):
    if s.attributes['id'].value[:4] == "edge":
        for thing in s.childNodes:
            if thing.nodeType == s.ELEMENT_NODE and thing.tagName == "path":
                thing.setAttribute("inkscape:connector-type", "polyline")
                thing.setAttribute("inkscape:connector-curvature", "3")
                nodeID = nodeTitles[edgeTitles[s.attributes['id'].value][0]]
                thing.setAttribute("inkscape:connection-start", "#" + nodeID)
                nodeID = nodeTitles[edgeTitles[s.attributes['id'].value][1]]
                thing.setAttribute("inkscape:connection-end", "#" + nodeID)

# 6. Output should have draggable nodes in Inkscape, after ungrouping as needed.
graphvizSVGFile.writexml(sys.stdout)
