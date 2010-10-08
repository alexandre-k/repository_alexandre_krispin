/******************************************************************************
 *            Copyright (c) 2006-2009 Michel Gutierrez. All Rights Reserved.
 ******************************************************************************/

/**
* @fileoverview Accessing global utilities
* @author mig
* @version 1.0
*/

/**
 * Constants.
 */
const NS_UTIL_SERVICE_CID = Components.ID("{dbd8dc72-2cdf-44ad-bf9a-5dc7a3fc3036}");
const NS_UTIL_SERVICE_PROG_ID = "@downloadhelper.net/util-service;1";

Node=null;
XPathResult=null;

/**
* Service constructor
* @class Service accessing global utilities
*/
function UtilService() {
	this.stringBundle=Components.classes["@mozilla.org/intl/stringbundle;1"].getService().
		QueryInterface(Components.interfaces.nsIStringBundleService).
			createBundle("chrome://dwhelper/locale/strings.properties");
	this.RDF = Components.classes["@mozilla.org/rdf/rdf-service;1"].getService().
		QueryInterface(Components.interfaces.nsIRDFService);
	this.RDFCUtils = Components.classes["@mozilla.org/rdf/container-utils;1"].getService().
		QueryInterface(Components.interfaces.nsIRDFContainerUtils);
}

UtilService.prototype = {
}

UtilService.prototype.getRDF = function() {
	return this.RDF;
}

UtilService.prototype.getRDFCUtils = function() {
	return this.RDFCUtils;
}

UtilService.prototype.getText = function(name) {
	try {
		return this.stringBundle.GetStringFromName(name);
	} catch(e) {
		return name;
	}
}

UtilService.prototype.getFText=function(name,params,length) {
	if(params==null)
		params=[];
	try {
		return this.stringBundle.formatStringFromName(name,params,params.length);
	} catch(e) {
		return name;
	}
}

UtilService.prototype.getVersion=function(uuid) {
	var RDF = Components.classes["@mozilla.org/rdf/rdf-service;1"].getService().
		QueryInterface(Components.interfaces.nsIRDFService);
	var RDFCUtils = Components.classes["@mozilla.org/rdf/container-utils;1"].getService().
		QueryInterface(Components.interfaces.nsIRDFContainerUtils);
	var extMgr=Components.classes["@mozilla.org/extensions/manager;1"].
		getService(Components.interfaces.nsIExtensionManager);		
	var target=extMgr.datasource.GetTarget(
		RDF.GetResource("urn:mozilla:item:"+uuid),
		RDF.GetResource("http://www.mozilla.org/2004/em-rdf#version"),
		true);
	if(target==null)
		return null;
	return target.QueryInterface(Components.interfaces.nsIRDFLiteral).Value;
}

UtilService.prototype.getPropertyValue = function(ds,res,prop) {
	var target=ds.GetTarget(res,prop,true);
	if(target==null)
		return null;
	return target.QueryInterface(Components.interfaces.nsIRDFLiteral).Value;
}

UtilService.prototype.getPropertyValueRS = function(ds,res,prop) {
	return this.getPropertyValue(ds,res,this.RDF.GetResource(prop));
}

UtilService.prototype.getPropertyValueSR = function(ds,res,prop) {
	return this.getPropertyValue(ds,this.RDF.GetResource(res),prop);
}

UtilService.prototype.getPropertyValueSS = function(ds,res,prop) {
	return this.getPropertyValue(ds,this.RDF.GetResource(res),this.RDF.GetResource(prop));
}

UtilService.prototype.setPropertyValue = function(ds,res,prop,value) {
	value=this.RDF.GetLiteral(value);
	this.removeProperties(ds,res,prop);
	if(value!=null) {
		ds.Assert(res,prop,value,true);
	}
}

UtilService.prototype.setPropertyValueSR = function(ds,res,prop,value) {
	this.setPropertyValue(ds,this.RDF.GetResource(res),prop,value);
}

UtilService.prototype.setPropertyValueRS = function(ds,res,prop,value) {
	this.setPropertyValue(ds,res,this.RDF.GetResource(prop),value);
}

UtilService.prototype.setPropertyValueSS = function(ds,res,prop,value) {
	this.setPropertyValue(ds,this.RDF.GetResource(res),this.RDF.GetResource(prop),value);
}

UtilService.prototype.removeProperties = function(ds,res,prop) {
	var i=ds.GetTargets(res,prop,true);
	var targets=[];
	while(i.hasMoreElements()) {
		targets.push(i.getNext());
	}
	for(var i=0;i<targets.length;i++) {
		ds.Unassert(res,prop,targets[i]);
	}
}

UtilService.prototype.removePropertiesSR = function(ds,res,prop) {
	this.removeProperties(ds,this.RDF.GetResource(res),prop);
}

UtilService.prototype.removePropertiesRS = function(ds,res,prop) {
	this.removeProperties(ds,res,this.RDF.GetResource(prop));
}

UtilService.prototype.removePropertiesSS = function(ds,res,prop) {
	this.removeProperties(ds,this.RDF.GetResource(res),this.RDF.GetResource(prop));
}

UtilService.prototype.getChildResources = function(ds,res,count) {
	var children=[];
	var seq=this.RDFCUtils.MakeSeq(ds,res);
	var j=seq.GetElements();
	while(j.hasMoreElements()) {
		var li=j.getNext().QueryInterface(Components.interfaces.nsIRDFResource);
		children.push(li);
	}
	count.value=children.length;
	return children;
}

UtilService.prototype.getChildResourcesS = function(ds,res,count) {
	return this.getChildResources(ds,this.RDF.GetResource(res),count);
}

UtilService.prototype.getPropertyResource = function(ds,res,prop) {
	var target=ds.GetTarget(res,prop,true);
	if(target==null)
		return null;
	return target.QueryInterface(Components.interfaces.nsIRDFResource);
}

UtilService.prototype.getPropertyResourceRS = function(ds,res,prop) {
	return this.getPropertyResource(ds,res,this.RDF.GetResource(prop));
}

UtilService.prototype.getPropertyResourceSR = function(ds,res,prop) {
	return this.getPropertyResource(ds,this.RDF.GetResource(res),prop);
}

UtilService.prototype.getPropertyResourceSS = function(ds,res,prop) {
	return this.getPropertyResource(ds,this.RDF.GetResource(res),this.RDF.GetResource(prop));
}

UtilService.prototype.setPropertyResource = function(ds,res,prop,value) {
	this.removeProperties(ds,res,prop);
	if(value!=null) {
		ds.Assert(res,prop,value,true);
	}
}

UtilService.prototype.setPropertyResourceRRS = function(ds,res,prop,value) {
	this.setPropertyResource(ds,res,prop,this.RDF.GetResource(value));
}

UtilService.prototype.setPropertyResourceRSR = function(ds,res,prop,value) {
	this.setPropertyResource(ds,res,this.RDF.GetResource(prop),value);
}

UtilService.prototype.setPropertyResourceRSS = function(ds,res,prop,value) {
	this.setPropertyResource(ds,res,this.RDF.GetResource(prop),this.RDF.GetResource(value));
}

UtilService.prototype.setPropertyResourceSRR = function(ds,res,prop,value) {
	this.setPropertyResource(ds,this.RDF.GetResource(res),prop,value);
}

UtilService.prototype.setPropertyResourceSRS = function(ds,res,prop,value) {
	this.setPropertyResource(ds,this.RDF.GetResource(res),prop,this.RDF.GetResource(value));
}

UtilService.prototype.setPropertyResourceSSR = function(ds,res,prop,value) {
	this.setPropertyResource(ds,this.RDF.GetResource(res),this.RDF.GetResource(prop),value);
}

UtilService.prototype.setPropertyResourceSSS = function(ds,res,prop,value) {
	this.setPropertyResource(ds,this.RDF.GetResource(res),this.RDF.GetResource(prop),this.RDF.GetResource(value));
}

UtilService.prototype.createNode=function(ds,parentNode,res) {
	var node=res;
	if(node==null) {
		node=this.RDF.GetAnonymousResource();
	} 
	this.setPropertyResourceRSS(ds,node,
		"http://www.w3.org/1999/02/22-rdf-syntax-ns#instanceOf",
		"http://www.w3.org/1999/02/22-rdf-syntax-ns#Seq");
	this.setPropertyValueRS(ds,node,
		"http://www.w3.org/1999/02/22-rdf-syntax-ns#nextVal","1");
	
	if(parentNode!=null) {
		parentNode=this.RDFCUtils.MakeSeq(ds,parentNode);
		parentNode.AppendElement(node);
	} 
	return node;
}

UtilService.prototype.createNodeRS=function(ds,parentNode,resValue) {
	if(resValue!=null)
		resValue=this.RDF.GetResource(resValue);
	return this.createNode(ds,parentNode,resValue);
}

UtilService.prototype.createNodeSR=function(ds,parentNode,resValue) {
	if(parentNode!=null)
		parentNode=this.RDF.GetResource(parentNode);
	return this.createNode(ds,parentNode,resValue);
}

UtilService.prototype.createNodeSS=function(ds,parentNode,resValue) {
	if(parentNode!=null)
		parentNode=this.RDF.GetResource(parentNode);
	if(resValue!=null)
		resValue=this.RDF.GetResource(resValue);
	return this.createNode(ds,parentNode,resValue);
}

UtilService.prototype.createAnonymousNode=function(ds,parentNode) {
	return this.createNode(ds,parentNode,null);
}

UtilService.prototype.createAnonymousNodeS=function(ds,parentNode) {
	return this.createAnonymousNode(ds,this.RDF.GetResource(parentNode));
}

UtilService.prototype.createRootNode=function(ds,res) {
	return this.createNode(ds,null,res);
}

UtilService.prototype.createRootNodeS=function(ds,resValue) {
	return this.createRootNode(ds,this.RDF.GetResource(resValue));
}

UtilService.prototype.createAnonymousRootNode=function(ds) {
	return this.createNode(ds,null,null);
}

UtilService.prototype.getDatasourceFromRDFFile=function(file) {

	var fis = Components.classes['@mozilla.org/network/file-input-stream;1'].
	    createInstance(Components.interfaces.nsIFileInputStream);
	fis.init(file,1,0,false);
	
	var charset = "UTF-8";
	const replacementChar = Components.interfaces.nsIConverterInputStream.DEFAULT_REPLACEMENT_CHARACTER;
	var is = Components.classes["@mozilla.org/intl/converter-input-stream;1"]
	                   .createInstance(Components.interfaces.nsIConverterInputStream);
	is.init(fis, charset, 1024, replacementChar);
	
	var fileContents="";
	var str = {};
	while (is.readString(4096, str) != 0) {
	  fileContents+=str.value;
	}

	is.close();
	fis.close();
	
	var parser=Components.classes
	      	['@mozilla.org/rdf/xml-parser;1'].
	          	createInstance(Components.interfaces.nsIRDFXMLParser);
	var uri = Components.classes["@mozilla.org/network/standard-url;1"].
	            createInstance(Components.interfaces.nsIURI);
	uri.spec = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
	var memDS=Components.classes
	      	['@mozilla.org/rdf/datasource;1?name=in-memory-datasource'].
	          	createInstance(Components.interfaces.nsIRDFDataSource);
	parser.parseString(memDS,uri,fileContents);
	return memDS;
}

UtilService.prototype.dumpDatasource=function(ds) {
	if(ds==null)
		return;
	var i = ds.GetAllResources();
	while(i.hasMoreElements()) {
		var source = i.getNext();
		var j = ds.ArcLabelsOut(source);
		while(j.hasMoreElements()) {
			var predicate = j.getNext();
			var k = ds.GetTargets(source,predicate,true);
			while(k.hasMoreElements()) {
				var target = k.getNext();
				source=source.QueryInterface(Components.interfaces.nsIRDFResource);
				predicate=predicate.QueryInterface(Components.interfaces.nsIRDFResource);
				try {
					target=target.QueryInterface(Components.interfaces.nsIRDFResource);
				} catch(e) {
					target=target.QueryInterface(Components.interfaces.nsIRDFLiteral);
				}
				dump(source.Value+" - "+predicate.Value+" - "+target.Value+"\n");
			}
		}
	}
}

UtilService.prototype.exceptionDesc=function(e) {
	e=e.QueryInterface(Components.interfaces.nsIException);
	return e.message+" "+e.filename+":"+e.lineNumber;
}

UtilService.prototype.getDatasource=function(tree) {
	if(tree.database==null)
		return null;
	var i=tree.database.GetDataSources();
	if(i.hasMoreElements()) {
		return i.getNext().QueryInterface(Components.interfaces.nsIRDFDataSource);
	} else {
		return null;
	}
}

UtilService.prototype.clearDatasource=function(tree) {
	if(tree.database==null)
		return;
	var dss=[];
	var i=tree.database.GetDataSources();
	while(i.hasMoreElements()) {
		dss.push(i.getNext());
	}
	for(var i=0;i<dss.length;i++) {
		var ds = dss[i].QueryInterface(Components.interfaces.nsIRDFDataSource);
		var i = ds.GetAllResources();
		ds.beginUpdateBatch();
		while(i.hasMoreElements()) {
			var source = i.getNext();
			var j = ds.ArcLabelsOut(source);
			while(j.hasMoreElements()) {
				var predicate = j.getNext();
				var k = ds.GetTargets(source,predicate,true);
				while(k.hasMoreElements()) {
					var target = k.getNext();
					ds.Unassert(source,predicate,target);
				}
			}
		}
		ds.endUpdateBatch();
	}
	for(var i=0;i<dss.length;i++) {
		var ds = dss[i].QueryInterface(Components.interfaces.nsIRDFDataSource);
		tree.database.RemoveDataSource(ds);
	}
}

UtilService.prototype.emptyDatasource=function(ds) {
	var i = ds.GetAllResources();
	ds.beginUpdateBatch();
	while(i.hasMoreElements()) {
		var source = i.getNext();
		var j = ds.ArcLabelsOut(source);
		while(j.hasMoreElements()) {
			var predicate = j.getNext();
			var k = ds.GetTargets(source,predicate,true);
			while(k.hasMoreElements()) {
				var target = k.getNext();
				ds.Unassert(source,predicate,target);
			}
		}
	}
	ds.endUpdateBatch();
}

UtilService.prototype.removeDatasources=function(tree) {
	if(tree.database==null)
		return;
	var dss=[];
	var i=tree.builder.database.GetDataSources();
	while(i.hasMoreElements()) {
		dss.push(i.getNext());
	}
	for(var i=0;i<dss.length;i++) {
		var ds = dss[i].QueryInterface(Components.interfaces.nsIRDFDataSource);
		tree.builder.database.RemoveDataSource(ds);
	}
}

UtilService.prototype.setDatasource=function(tree,ds) {
	this.removeDatasources(tree);
	if(ds!=null)
		tree.builder.database.AddDataSource(ds);
	tree.builder.rebuild();
}

UtilService.prototype.addDatasource=function(tree,ds) {
	if(ds!=null)
		tree.builder.database.AddDataSource(ds);
	tree.builder.rebuild();
}

UtilService.prototype.copyDatasource=function(ds) {
	var memDS=Components.classes
	        ['@mozilla.org/rdf/datasource;1?name=in-memory-datasource'].
	            createInstance(Components.interfaces.nsIRDFDataSource);
	var i = ds.GetAllResources();
	memDS.beginUpdateBatch();
	while(i.hasMoreElements()) {
		var source = i.getNext();
		var j = ds.ArcLabelsOut(source);
		while(j.hasMoreElements()) {
			var predicate = j.getNext();
			var k = ds.GetTargets(source,predicate,true);
			while(k.hasMoreElements()) {
				var target = k.getNext();
				memDS.Assert(source,predicate,target,true);
			}
		}
	}
	memDS.endUpdateBatch();
	
	return memDS;
}

UtilService.prototype.concatDatasource=function(ds0,ds) {
	var i = ds.GetAllResources();
	ds0.beginUpdateBatch();
	while(i.hasMoreElements()) {
		var source = i.getNext();
		var j = ds.ArcLabelsOut(source);
		while(j.hasMoreElements()) {
			var predicate = j.getNext();
			var k = ds.GetTargets(source,predicate,true);
			while(k.hasMoreElements()) {
				var target = k.getNext();
				ds0.Assert(source,predicate,target,true);
			}
		}
	}
	ds0.endUpdateBatch();
}

UtilService.prototype.getParentNode=function(ds,node) {
	var iter=ds.ArcLabelsIn(node);
	while(iter.hasMoreElements()) {
	    var arc=iter.getNext().QueryInterface(Components.interfaces.nsIRDFResource);
	    if(ChartletUtil.startsWith(arc.Value,"http://www.w3.org/1999/02/22-rdf-syntax-ns#_")) {
	       	node = ds.GetSource(arc,node,true);
	       	return node;
	    }
	}
	return null;
}

UtilService.prototype.getParentNodeS=function(ds,node) {
	return this.getParentNode(ds,this.RDF.GetResource(node));
}

UtilService.prototype.appendNode=function(ds,parentNode,node) {
	parentNode=this.RDFCUtils.MakeSeq(ds,parentNode);
	parentNode.AppendElement(node);
}

UtilService.prototype.appendNodeRS=function(ds,parentNode,node) {
	this.appendNode(ds,parentNode,this.RDF.GetResource(node));
}

UtilService.prototype.appendNodeSR=function(ds,parentNode,node) {
	this.appendNode(ds,this.RDF.GetResource(parentNode),node);
}

UtilService.prototype.appendNodeSS=function(ds,parentNode,node) {
	this.appendNode(ds,this.RDF.GetResource(parentNode),this.RDF.GetResource(node));
}

UtilService.prototype.removeReference = function(ds,res) {
	var seqElems=[];
	
	var deltriplet=[];
	var i=ds.ArcLabelsIn(res);
	while(i.hasMoreElements()) {
	    var arc=i.getNext().QueryInterface(Components.interfaces.nsIRDFResource);
	    var j=ds.GetSources(arc,res,true);
	    while(j.hasMoreElements()) {
	    	var source=j.getNext().QueryInterface(Components.interfaces.nsIRDFResource);
	    	var m=/^http\:\/\/www\.w3\.org\/1999\/02\/22\-rdf\-syntax\-ns#_([1-9][0-9]*)$/.exec(arc.Value);
	    	if(m.length==2) {
	    		var seq=ChartletUtil.RDFCUtils.MakeSeq(ds,source);
				seqElems.push({seq: seq, elem: res});    		
	    	} else {
		    	deltriplet.push({
		    		source: source,
		    		property: arc,
		    		target: res
	    		});
	    	}
	    }
	}
	
	var i=ds.ArcLabelsOut(res);
	while(i.hasMoreElements()) {
	    var arc=i.getNext().QueryInterface(Components.interfaces.nsIRDFResource);
	    var j=ds.GetTargets(res,arc,true);
	    while(j.hasMoreElements()) {
	    	var target=j.getNext().QueryInterface(Components.interfaces.nsIRDFNode);
	    	deltriplet.push({
	    		source: res,
	    		property: arc,
	    		target: target
	    		});
	    }
	}
	
	ds.beginUpdateBatch();
	for(var i=0;i<deltriplet.length;i++) {
		var triplet=deltriplet[i];
		ds.Unassert(triplet.source,triplet.property,triplet.target);
	}
	for(var i=0;i<seqElems.length;i++) {
		seqElems[i].seq.RemoveElement(seqElems[i].elem,true);
	}
	ds.endUpdateBatch();
}

UtilService.prototype.removeReferenceS = function(ds,res) {
	this.removeReference(ds,this.RDF.GetResource(res));
}

UtilService.prototype.xpGetSingleNode = function(node,xpath) {
	var anode=node.ownerDocument.evaluate(xpath,node,null,
		XPathResult.FIRST_ORDERED_NODE_TYPE,null).singleNodeValue;
	return anode;
}

UtilService.prototype.xpGetNodes = function(node,xpath,count) {
	var nodes=[];
    var xpr=node.ownerDocument.evaluate(xpath,
        node,null,
        XPathResult.ORDERED_NODE_ITERATOR_TYPE,
        null);
    var node0=xpr.iterateNext();
    while(node0!=null) {
    	nodes.push(node0);
        node0=xpr.iterateNext();
    }
    count.value=nodes.length;
	return nodes;
}

UtilService.prototype.xpGetStrings = function(node,xpath,count) {
	var strings=[];
    var xpr=node.ownerDocument.evaluate(xpath,
        node,null,
        XPathResult.ORDERED_NODE_ITERATOR_TYPE,
        null);
    var lastTextNode=null;
    var node0=xpr.iterateNext();
    while(node0!=null) {
    	if(node0.nodeType==Node.TEXT_NODE) {
    		if(node0.previousSibling==lastTextNode) {
    			var prevText=strings.pop();
    			strings.push(prevText+node0.nodeValue);
    		} else {
    			strings.push(node0.nodeValue);
    		}
    		lastTextNode=node0;
    	}
    	else if(node0.firstChild!=null && node0.firstChild.nodeType==Node.TEXT_NODE)
    		strings.push(node0.firstChild.nodeValue);
        node0=xpr.iterateNext();
    }
    count.value=strings.length;
	return strings;
}

UtilService.prototype.xpGetString = function(node,xpath) {
	var text=node.ownerDocument.evaluate(xpath,node,null,
		XPathResult.STRING_TYPE,null).stringValue;
	return text;
}

UtilService.prototype.getProfileDir=function() {
	var file = Components.classes["@mozilla.org/file/directory_service;1"]
    	.getService(Components.interfaces.nsIProperties)
        .get("ProfD", Components.interfaces.nsIFile);
    return file;
}

UtilService.prototype.getProfilePath=function() {
	return this.getProfileDir().path;
}

UtilService.prototype.getExtensionDir=function(uuid) {
	var file = this.getProfileDir();
	file.append("extensions");
	file.append(uuid);
	return file;
}

UtilService.prototype.getExtensionPath=function(uuid) {
	return this.getExtensionDir(uuid).path;
}

UtilService.prototype.removeReferenceS=function(ds,res) {
	this.removeReference(ds,this.RDF.GetResource(res));
}

UtilService.prototype.removeReference=function(ds,res) {

	var seqElems=[];
	
	var deltriplet=[];
	var i=ds.ArcLabelsIn(res);
	while(i.hasMoreElements()) {
	    var arc=i.getNext().QueryInterface(Components.interfaces.nsIRDFResource);
	    var j=ds.GetSources(arc,res,true);
	    while(j.hasMoreElements()) {
	    	var source=j.getNext().QueryInterface(Components.interfaces.nsIRDFResource);
	    	var m=/^http\:\/\/www\.w3\.org\/1999\/02\/22\-rdf\-syntax\-ns#_([1-9][0-9]*)$/.exec(arc.Value);
	    	if(m.length==2) {
	    		var seq=this.RDFCUtils.MakeSeq(ds,source);
				seqElems.push({seq: seq, elem: res});    		
	    	} else {
		    	deltriplet.push({
		    		source: source,
		    		property: arc,
		    		target: res
	    		});
	    	}
	    }
	}
	
	var i=ds.ArcLabelsOut(res);
	while(i.hasMoreElements()) {
	    var arc=i.getNext().QueryInterface(Components.interfaces.nsIRDFResource);
	    var j=ds.GetTargets(res,arc,true);
	    while(j.hasMoreElements()) {
	    	var target=j.getNext().QueryInterface(Components.interfaces.nsIRDFNode);
	    	deltriplet.push({
	    		source: res,
	    		property: arc,
	    		target: target
	    		});
	    }
	}
	
	ds.beginUpdateBatch();
	for(var i=0;i<deltriplet.length;i++) {
		var triplet=deltriplet[i];
		ds.Unassert(triplet.source,triplet.property,triplet.target);
	}
	for(var i=0;i<seqElems.length;i++) {
		seqElems[i].seq.RemoveElement(seqElems[i].elem,true);
	}
	ds.endUpdateBatch();
}

UtilService.prototype.generateUuid=function() {
	var uuid="";
	var groups=[8,4,4,4,12];
	for(var i=0;i<groups.length;i++) {
		if(i>0)
			uuid+="-";
		for(var j=0;j<groups[i];j++) {
			var d=Math.floor(Math.random()*16);
			if(d<10) {
				uuid+=String.fromCharCode("0".charCodeAt(0)+d);
			} else {
				uuid+=String.fromCharCode("a".charCodeAt(0)-10+d);
			}
		}
	}
	return uuid;
}

UtilService.prototype.generateXPath = function(node) {
	var node0=node;
	var str="";
	while(node.parentNode!=null) {
		var str0="/";
        if(node.nodeType==Node.TEXT_NODE) {
            str0+="text()";
        } else {
            str0+=node.nodeName.toLowerCase();
		}
		var index=this.getNodeChildIndex(node);
        str0+="["+(index+1)+"]";
		str=str0+str;
		node=node.parentNode;
	}
	str=node.nodeName+str;
	if(str.substr(0,9)=="#document")
		str=str.substring(9);
	return str;
}

UtilService.prototype.getNodeChildIndex = function(child) {
    var i=0;
    var parent=child.parentNode;
    var node=parent.firstChild;
    while(node!=null) {
        if(node==child)
            return i;
        if(node.nodeName==child.nodeName && 
        	(node.nodeType==Node.ELEMENT_NODE || node.nodeType==Node.TEXT_NODE))
            i++;
        node=node.nextSibling;
    }
    return -1;
}

UtilService.prototype.decodeURL = function(text) {
	var decoder=Components.classes["@mozilla.org/intl/utf8converterservice;1"]
		.getService(Components.interfaces.nsIUTF8ConverterService);
	text=decoder.convertURISpecToUTF8(text,null);
	var hc="0123456789ABCDEFabcdef"; 
	var pt="";
	var i=0;
	while (i < text.length) {
    	var ch=text.charAt(i);
    	if (ch=="+") {
	    	pt+=" ";
			i++;
		} else if (ch=="%") {
			if (i < (text.length-2) 
					&& hc.indexOf(text.charAt(i+1)) != -1 
					&& hc.indexOf(text.charAt(i+2)) != -1 ) {
				pt+=unescape( text.substr(i,3) );
				i+=3;
			} else {
				pt+="?";
				i++;
			}
		} else {
		   pt+=ch;
		   i++;
		}
	} 
   return pt;
}

UtilService.prototype.encodeURL = function(text) {
	var sc="0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-_.!~*'()";
	var h="0123456789ABCDEF";
	var encoded="";
	for (var i=0; i < text.length; i++ ) {
		var ch=text.charAt(i);
	    if (ch==" ") {
		    encoded+="+";
		} else if (sc.indexOf(ch) != -1) {
		    encoded+=ch;
		} else {
		    var charCode=ch.charCodeAt(0);
			if (charCode > 255) {
				encoded+="+";
			} else {
				encoded+="%"+h.charAt((charCode >> 4) & 0xF)+h.charAt(charCode & 0xF);
			}
		}
	}
	return encoded;
};

UtilService.prototype.contentPost = function(url,body,target,targetName,features) {
	if(target==null) {
		var wwatch = Components.classes["@mozilla.org/embedcomp/window-watcher;1"].getService().
			QueryInterface(Components.interfaces.nsIWindowWatcher);
		target=wwatch.openWindow(null, "about:blank",
	                 targetName, features, null);
	}
	var webNav = target.QueryInterface(Components.interfaces.nsIInterfaceRequestor).
		getInterface(Components.interfaces.nsIWebNavigation);
	var sis = null;
	if(body!=null) {
		sis=Components.classes["@mozilla.org/io/string-input-stream;1"].
        	createInstance(Components.interfaces.nsIStringInputStream);
        sis.setData("Content-length: "+body.length+"\r\n\r\n"+body,-1);
    }
	webNav.loadURI(url,webNav.LOAD_FLAGS_NONE,null,sis,null);
}

UtilService.prototype.xmlEscape = function(str) {
	str=str.replace(/&/g,"&amp;");
	str=str.replace(/</g,"&lt;");
	str=str.replace(/>/g,"&gt;");
	//return str;
	str=str.replace(/'/g,"&apos;");
	str=str.replace(/"/g,"&quot;");
	return str;
	var str0="";
	for(var i=0;i<str.length;i++) {
		var cc=str.charCodeAt(i);
		if(cc>=0x80)
			str0+="&#"+cc+";";
		else
			str0+=str[i];
	}
	return str0;
}

UtilService.prototype.getUTCTime = function() {
	var date=new Date();
	var t=date.getTime();
	t+=date.getTimezoneOffset()*60*1000;
	return t;
}

UtilService.prototype.formatNumberMinDigits = function(value,digits) {
	var str=""+value;
	while(str.length<digits)
		str="0"+str;
	return str;
}

UtilService.prototype.getExceptionStack = function(e) {
	try {
		var msg="";
		var location=e.location;
		while(location!=null) {
			msg+=location.filename+":"+location.lineNumber+"\n";
			location=location.caller;
		}
		return msg;
	} catch(e0) {
		return "[UtilService] error while getting exception frame: "+e0;
	}
}

UtilService.prototype.removeChild = function(ds,parent,child) {
	var seq=this.RDFCUtils.MakeSeq(ds,parent);
	seq.RemoveElement(child,true);
}

UtilService.prototype.removeChildRS = function(ds,parent,child) {
	var seq=this.RDFCUtils.MakeSeq(ds,parent);
	seq.RemoveElement(this.RDF.GetResource(child),true);
}

UtilService.prototype.removeChildSR = function(ds,parent,child) {
	var seq=this.RDFCUtils.MakeSeq(ds,this.RDF.GetResource(parent));
	seq.RemoveElement(child,true);
}

UtilService.prototype.removeChildSS = function(ds,parent,child) {
	var seq=this.RDFCUtils.MakeSeq(ds,this.RDF.GetResource(parent));
	seq.RemoveElement(this.RDF.GetResource(child),true);
}

UtilService.prototype.getRDFNodeValue = function(node) {
	if(node==null)
		return null;
	try {
		var res=node.QueryInterface(Components.interfaces.nsIRDFResource);
		return res.Value;
	} catch(e) {}
	try {
		var lit=node.QueryInterface(Components.interfaces.nsIRDFLiteral);
		return lit.Value;
	} catch(e) {}
	return null;
}

UtilService.prototype.getUnicharPref = function(pref, prefName, defValue) {
	//dump("getUnicharPref("+prefName+"): default="+defValue+"\n");
	var value=defValue;
	try {
		value=pref.getComplexValue(prefName,Components.interfaces.nsISupportsString).data;
	} catch(e) {}
	//dump("getUnicharPref("+prefName+")=>"+value+"\n");
	return value;
}

UtilService.prototype.setUnicharPref = function(pref, prefName, value) {
	//dump("setUnicharPref("+prefName+"): value="+value+"\n");
	var str = Components.classes["@mozilla.org/supports-string;1"].
		createInstance(Components.interfaces.nsISupportsString);
	str.data = value; 
	pref.setComplexValue(prefName,Components.interfaces.nsISupportsString,str);
	//dump("setUnicharPref("+prefName+"): done - check="+this.getUnicharPref(pref,prefName,null)+"\n");
}

UtilService.prototype.getStringSupport = function(str) {
	var strSupport=Components.classes["@mozilla.org/supports-string;1"].
		createInstance(Components.interfaces.nsISupportsString);
	strSupport.data=str;
	return strSupport;
}

UtilService.prototype.setPropsString = function(props, key, value) {
	var strSupport=Components.classes["@mozilla.org/supports-string;1"].
		createInstance(Components.interfaces.nsISupportsString);
	strSupport.data=value;
	props.set(key,strSupport);
}

UtilService.prototype.getPropsString = function(props, key) {
	try {
		var value=props.get(key,Components.interfaces.nsISupportsString);
		if(value)
			return value.toString();
	} catch(e) {}
	return null;
}

UtilService.prototype.alert = function(message,title) {
	var promptService=Components.classes["@mozilla.org/embedcomp/prompt-service;1"].getService(Components.interfaces.nsIPromptService);
	promptService.alert(null,title,message);
}

UtilService.prototype.alertError = function(message) {
	this.alert(message,this.getText("alert.title.error"));
}

UtilService.prototype.alertWarning = function(message) {
	this.alert(message,this.getText("alert.title.warning"));
}

var Base64 = {
		// private property
		_keyStr : "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=",
		// public method for encoding
		encode : function (input) {
			var output = "";
			var chr1, chr2, chr3, enc1, enc2, enc3, enc4;
			var i = 0;
			input = Base64._utf8_encode(input);
			while (i < input.length) {
				chr1 = input.charCodeAt(i++);
				chr2 = input.charCodeAt(i++);
				chr3 = input.charCodeAt(i++);
				enc1 = chr1 >> 2;
				enc2 = ((chr1 & 3) << 4) | (chr2 >> 4);
				enc3 = ((chr2 & 15) << 2) | (chr3 >> 6);
				enc4 = chr3 & 63;
				if (isNaN(chr2)) {
					enc3 = enc4 = 64;
				} else if (isNaN(chr3)) {
					enc4 = 64;
				}
				output = output +
				this._keyStr.charAt(enc1) + this._keyStr.charAt(enc2) +
				this._keyStr.charAt(enc3) + this._keyStr.charAt(enc4);
			}
			return output;
		},
	 
		// public method for decoding
		decode : function (input) {
			var output = "";
			var chr1, chr2, chr3;
			var enc1, enc2, enc3, enc4;
			var i = 0;
			input = input.replace(/[^A-Za-z0-9\+\/\=]/g, "");
			while (i < input.length) {
				enc1 = this._keyStr.indexOf(input.charAt(i++));
				enc2 = this._keyStr.indexOf(input.charAt(i++));
				enc3 = this._keyStr.indexOf(input.charAt(i++));
				enc4 = this._keyStr.indexOf(input.charAt(i++));
				chr1 = (enc1 << 2) | (enc2 >> 4);
				chr2 = ((enc2 & 15) << 4) | (enc3 >> 2);
				chr3 = ((enc3 & 3) << 6) | enc4;
				output = output + String.fromCharCode(chr1);
				if (enc3 != 64) {
					output = output + String.fromCharCode(chr2);
				}
				if (enc4 != 64) {
					output = output + String.fromCharCode(chr3);
				}
			}
			output = Base64._utf8_decode(output);
			return output;
		},
	 
		// private method for UTF-8 encoding
		_utf8_encode : function (string) {
			string = string.replace(/\r\n/g,"\n");
			var utftext = "";
			for (var n = 0; n < string.length; n++) {
				var c = string.charCodeAt(n);
				if (c < 128) {
					utftext += String.fromCharCode(c);
				}
				else if((c > 127) && (c < 2048)) {
					utftext += String.fromCharCode((c >> 6) | 192);
					utftext += String.fromCharCode((c & 63) | 128);
				}
				else {
					utftext += String.fromCharCode((c >> 12) | 224);
					utftext += String.fromCharCode(((c >> 6) & 63) | 128);
					utftext += String.fromCharCode((c & 63) | 128);
				}
			}
			return utftext;
		},
		// private method for UTF-8 decoding
		_utf8_decode : function (utftext) {
			var string = "";
			var i = 0;
			var c = c1 = c2 = 0;
			while ( i < utftext.length ) {
				c = utftext.charCodeAt(i);
				if (c < 128) {
					string += String.fromCharCode(c);
					i++;
				}
				else if((c > 191) && (c < 224)) {
					c2 = utftext.charCodeAt(i+1);
					string += String.fromCharCode(((c & 31) << 6) | (c2 & 63));
					i += 2;
				}
				else {
					c2 = utftext.charCodeAt(i+1);
					c3 = utftext.charCodeAt(i+2);
					string += String.fromCharCode(((c & 15) << 12) | ((c2 & 63) << 6) | (c3 & 63));
					i += 3;
				}
			}
			return string;
		}
}

UtilService.prototype.base64Encode = function(text) {
	return Base64.encode(text);
}

UtilService.prototype.base64Decode = function(text) {
	return Base64.decode(text);
}

UtilService.prototype.migratePasswords=function() {
	var prefService=Components.classes["@mozilla.org/preferences-service;1"]
	                                   .getService(Components.interfaces.nsIPrefService);
	var pref=prefService.getBranch("dwhelper.");
	if(pref.getBoolPref("passwords-migrated")==true) {
		return true;
	}
	try {
		var mp3tunesPw=pref.getCharPref("mp3tunes.password");
		if(mp3tunesPw.length>0) {
			this.doSetPassword("mp3tunes",mp3tunesPw);
			pref.setCharPref("mp3tunes.password","");
		}
		var twitterPw=pref.getCharPref("twitter.password");
		if(twitterPw.length>0) {
			this.doSetPassword("twitter",this.base64Decode(twitterPw));
			pref.setCharPref("twitter.password","");
		}
		pref.setBoolPref("passwords-migrated",true);
		return true;
	} catch(e) {
		dump("!!! [Core] migratePasswords: "+e+"\n");
	}
	return false;
}

UtilService.prototype.getPassword = function(service) {
	if(!this.migratePasswords())
		return null;
	return this.doGetPassword(service);
}
	
UtilService.prototype.doGetPassword = function(service) {
	if(Components.classes["@mozilla.org/login-manager;1"]) {
		var loginMgr = Components.classes["@mozilla.org/login-manager;1"]
		                                  .getService(Components.interfaces.nsILoginManager);
		var logins = loginMgr.findLogins({}, service+".password-manager.downloadhelper.net", null, 'downloadhelper');
        for (var i = 0; i < logins.length; i++) {
           if (logins[i].username == 'downloadhelper') {
              return logins[i].password;
           }
        }
        return null;
	} else {
		var prefService=Components.classes["@mozilla.org/preferences-service;1"]
		                                   .getService(Components.interfaces.nsIPrefService);
		var pref=prefService.getBranch("dwhelper.");
		try {
			return this.base64Decode(pref.getCharPref("password-manager."+service));
		} catch(e) {
			return null;
		}
	}
}

UtilService.prototype.setPassword = function(service,password) {
	if(!this.migratePasswords())
		return;
	this.doSetPassword(service,password);
}

UtilService.prototype.doSetPassword = function(service,password) {
	if(Components.classes["@mozilla.org/login-manager;1"]) {
		var loginMgr = Components.classes["@mozilla.org/login-manager;1"]
		                                  .getService(Components.interfaces.nsILoginManager);
		var logins = loginMgr.findLogins({}, service+".password-manager.downloadhelper.net", null, 'downloadhelper');
        for (var i = 0; i < logins.length; i++) {
        	loginMgr.removeLogin(logins[i]);
        }
		var nsLoginInfo = new Components.Constructor("@mozilla.org/login-manager/loginInfo;1",
                Components.interfaces.nsILoginInfo,
                "init");
		var loginInfo = new nsLoginInfo(service+'.password-manager.downloadhelper.net', null, 'downloadhelper', 'downloadhelper', password, "", "");
		try {
			loginMgr.addLogin(loginInfo);
		} catch(e) {
			dump("!!! [Util] doSetPassword('"+service+","+password+"): addLogin: "+e1+"\n");
		}
	} else {
		var prefService=Components.classes["@mozilla.org/preferences-service;1"]
		                                   .getService(Components.interfaces.nsIPrefService);
		var pref=prefService.getBranch("dwhelper.");
		pref.setCharPref("password-manager."+service,this.base64Encode(password));
	}
}

UtilService.prototype.priorTo19 = function() {
	var browserCompatible=false;
	try {
		var browserVersion=Components.classes["@mozilla.org/xre/app-info;1"]
				    		                   .getService(Components.interfaces.nsIXULAppInfo).platformVersion;
		var comparator=Components.classes["@mozilla.org/xpcom/version-comparator;1"]
		                                  .getService(Components.interfaces.nsIVersionComparator);
		if(comparator.compare(browserVersion,"1.9")>=0)
			browserCompatible=true;
	} catch(e) {}
	return !browserCompatible;
}

UtilService.prototype.QueryInterface = function(iid) {
    if (!iid.equals(Components.interfaces.nsISupports) && 
    	!iid.equals(Components.interfaces.dhIUtilService)) {
			//dump("UtilService: requested invalid interface "+iid+"\n");
            throw Components.results.NS_ERROR_NO_INTERFACE;
        }
    return this;
}

var vUtilServiceModule = {
    firstTime: true,
    
    /*
     * RegisterSelf is called at registration time (component installation
     * or the only-until-release startup autoregistration) and is responsible
     * for notifying the component manager of all components implemented in
     * this module.  The fileSpec, location and type parameters are mostly
     * opaque, and should be passed on to the registerComponent call
     * unmolested.
     */
    registerSelf: function (compMgr, fileSpec, location, type) {

        if (this.firstTime) {
            this.firstTime = false;
            throw Components.results.NS_ERROR_FACTORY_REGISTER_AGAIN;
        }
        compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
        compMgr.registerFactoryLocation(NS_UTIL_SERVICE_CID,
                                        "UtilService",
                                        NS_UTIL_SERVICE_PROG_ID, 
                                        fileSpec,
                                        location,
                                        type);
    },

	unregisterSelf: function(compMgr, fileSpec, location) {
    	compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    	compMgr.unregisterFactoryLocation(NS_UTIL_SERVICE_CID, fileSpec);
	},

    /*
     * The GetClassObject method is responsible for producing Factory and
     * SingletonFactory objects (the latter are specialized for services).
     */
    getClassObject: function (compMgr, cid, iid) {
        if (!cid.equals(NS_UTIL_SERVICE_CID)) {
	    	throw Components.results.NS_ERROR_NO_INTERFACE;
		}

        if (!iid.equals(Components.interfaces.nsIFactory)) {
	    	throw Components.results.NS_ERROR_NOT_IMPLEMENTED;
		}

        return this.vUtilServiceFactory;
    },

    /* factory object */
    vUtilServiceFactory: {
        /*
         * Construct an instance of the interface specified by iid, possibly
         * aggregating it with the provided outer.  (If you don't know what
         * aggregation is all about, you don't need to.  It reduces even the
         * mightiest of XPCOM warriors to snivelling cowards.)
         */
        createInstance: function (outer, iid) {
            if (outer != null) {
				throw Components.results.NS_ERROR_NO_AGGREGATION;
	    	}

			if(Node==null)
				Node=Components.interfaces.nsIDOMNode;

			if(XPathResult==null)
				XPathResult=Components.interfaces.nsIDOMXPathResult;
	
			return (new UtilService()).QueryInterface(iid);
        }
    },

    /*
     * The canUnload method signals that the component is about to be unloaded.
     * C++ components can return false to indicate that they don't wish to be
     * unloaded, but the return value from JS components' canUnload is ignored:
     * mark-and-sweep will keep everything around until it's no longer in use,
     * making unconditional ``unload'' safe.
     *
     * You still need to provide a (likely useless) canUnload method, though:
     * it's part of the nsIModule interface contract, and the JS loader _will_
     * call it.
     */
    canUnload: function(compMgr) {
		return true;
    }
};

function NSGetModule(compMgr, fileSpec) {
    return vUtilServiceModule;
}
