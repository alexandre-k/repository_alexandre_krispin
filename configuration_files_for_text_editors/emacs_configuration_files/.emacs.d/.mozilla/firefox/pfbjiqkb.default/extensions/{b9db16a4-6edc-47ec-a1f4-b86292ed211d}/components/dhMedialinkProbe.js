/******************************************************************************
 *            Copyright (c) 2006-2009 Michel Gutierrez. All Rights Reserved.
 ******************************************************************************/

/**
 * Constants.
 */

const NS_MLPROBE_CID = Components.ID("{7f3707a8-34b9-488d-a861-ffcb0a3e0c29}");
const NS_MLPROBE_PROG_ID = "@downloadhelper.net/medialink-probe;1";
const DHNS = "http://downloadhelper.net/1.0#";

var Util=null;

/**
* Object constructor
*/
function MLProbe() {
	try {
		//dump("[MLProbe] constructor\n");
		var prefService=Components.classes["@mozilla.org/preferences-service;1"]
		                       			.getService(Components.interfaces.nsIPrefService);
		this.pref=prefService.getBranch("dwhelper.");
		this.mediaPattern="jpg|jpeg|gif|png|mpg|mpeg|avi|rm|wmv|mov|flv";
		this.minFileCount=2;
		this.core=Components.classes["@downloadhelper.net/core;1"].
			getService(Components.interfaces.dhICore);
		this.core.registerProbe(this);
	} catch(e) {
		dump("[MLProbe] !!! constructor: "+e+"\n");
	}
}

MLProbe.prototype = {}

MLProbe.prototype.handleDocument=function(document,window) {
	try {
		//dump("[MLProbe] handleDocument("+document.URL+")\n");

		var medialinkEnabled=true;
		try {
			medialinkEnabled=this.pref.getBoolPref("enable-medialink-method");
		} catch(e) {
		}
		if(medialinkEnabled==false)
			return;

		var dom=document.documentElement;
		var classes={};
		var allHRefs={};
		var mediaNodes=[];
		var hitCount=0;
		var hitCountMax=this.pref.getIntPref("medialink-max-hits");
		var aNodes=Util.xpGetNodes(dom,".//a[@href]",{});
		for(var i=0;i<aNodes.length;i++) {
			var aNode=aNodes[i];
			var href=aNode.getAttribute("href");
			if(allHRefs[href]!=null)
				continue;
			allHRefs[href]="";
			var mediaPattern=this.mediaPattern;
			try {
				mediaPattern=this.pref.getCharPref("medialink-extensions");
			} catch(e) {}
			var mPatt=new RegExp("^.*\\.(?:"+mediaPattern+")$","i");
			if(mPatt.exec(href)==null)
				continue;
			mediaNodes.push(aNode);
			
			var hrefParts=/^(.*[^0-9])?([0-9]+)([^\/]*?\.[^\/]*?)$/.exec(href);
			if(hrefParts!=null && hrefParts.length==4) {
				if(hrefParts[1]==undefined)
					hrefParts[1]="";
				var key=hrefParts[1]+"$$$"+hrefParts[3];
				var group=classes[key];
				if(group==null) {
					group={
						nodes: [],
						ext: /.*\.(.*?)$/.exec(hrefParts[3])[1],
					};
					classes[key]=group;
				}
				var classNodes=group.nodes;
				classNodes.push(aNode);
			}
			hitCount++;
			if(isNaN(hitCountMax)==false && hitCountMax>0 && hitCount>=hitCountMax)
				break;
		}
		this.groups=[];
		var groupIndex=0;
		var maxNodeCount=0;
		for(var i in classes) {
			var group=classes[i];
			var classNodes=group.nodes;
			if(maxNodeCount<classNodes.length)
				maxNodeCount=classNodes.length;
			if(classNodes.length>=this.minFileCount) {
				var desc=this.getDesc(document,group.ext+" ("+classNodes.length+")",group.nodes)
				this.core.addEntryForDocument(desc,document,window);
			}
		}
		if(maxNodeCount<mediaNodes.length) {
			var desc=this.getDesc(document,Util.getText("menu.alllinkstomedia")+" ("+mediaNodes.length+")",mediaNodes);
			this.core.addEntryForDocument(desc,document,window);
		}
		return;
	} catch(e) {
		dump("!!! [MLProbe] handleDocument("+document.URL+"): "+Util.exceptionDesc(e)+"\n");
	}
}

MLProbe.prototype.getDesc = function(document,label,nodes) {
	var desc=Components.classes["@mozilla.org/properties;1"].
		createInstance(Components.interfaces.nsIProperties);
	Util.setPropsString(desc,"page-url",document.URL);
	Util.setPropsString(desc,"referrer",document.URL);
	Util.setPropsString(desc,"label",label);				
	Util.setPropsString(desc,"icon-url","chrome://dwhelper/skin/medialink.gif");
	Util.setPropsString(desc,"capture-method","medialink");
	Util.setPropsString(desc,"sn-preserve-label","yes");
	desc.set("mouse-listener",this);
	
	var fdescs=Components.classes["@mozilla.org/array;1"].
		createInstance(Components.interfaces.nsIMutableArray);
	desc.set("links",fdescs);
	for(var j=0;j<nodes.length;j++) {
		var node=nodes[j];
		var ndesc=Components.classes["@mozilla.org/properties;1"].
			createInstance(Components.interfaces.nsIProperties);
		var href=node.getAttribute("href");
	    var url = Components.classes["@mozilla.org/network/standard-url;1"].createInstance(Components.interfaces.nsIURI);
		url.spec = document.URL;
		var urlStr=url.resolve(href);
		Util.setPropsString(ndesc,"media-url",urlStr);
		ndesc.set("node",node);
		fdescs.appendElement(ndesc,false);
	}
	return desc;
}

MLProbe.prototype.mouseOver = function(desc) {
	//dump("[MLProbe] mouseOver()\n");
	try {
		if(desc.has("links")) {
			var links=desc.get("links",Components.interfaces.nsIArray);
			var i=links.enumerate();
			while(i.hasMoreElements()) {
				var ndesc=i.getNext().QueryInterface(Components.interfaces.nsIProperties);
				if(ndesc.has("node")) {
					var aNode=ndesc.get("node",Components.interfaces.nsIDOMNode);
					var oldBorder=aNode.style.border;
					aNode.setAttribute("dwhelper-border",oldBorder);
					aNode.style.border="5px dashed Red";
					var oldDisplay=aNode.style.display;
					aNode.setAttribute("dwhelper-display",oldDisplay);
					aNode.style.display="block";
				}
			}
		}
	} catch(e) {
		dump("!!! [MLProbe] mouseOver(): "+Util.exceptionDesc(e)+"\n");
	}
}

MLProbe.prototype.mouseOut = function(desc) {
	//dump("[MLProbe] mouseOut()\n");
	try {
		if(desc.has("links")) {
			var links=desc.get("links",Components.interfaces.nsIArray);
			var i=links.enumerate();
			while(i.hasMoreElements()) {
				var ndesc=i.getNext().QueryInterface(Components.interfaces.nsIProperties);
				if(ndesc.has("node")) {
					var aNode=ndesc.get("node",Components.interfaces.nsIDOMNode);
					var oldBorder=aNode.getAttribute("dwhelper-border");
					aNode.style.border=oldBorder;
					var oldDisplay=aNode.getAttribute("dwhelper-display");
					aNode.style.display=oldDisplay;
				}
			}
		}
	} catch(e) {
		dump("!!! [MLProbe] mouseOut(): "+Util.exceptionDesc(e)+"\n");
	}
}

MLProbe.prototype.handleRequest=function(request) {
}
	
MLProbe.prototype.handleResponse=function(request) {
}
	
MLProbe.prototype.QueryInterface = function(iid) {
	//dump("[MLProbe] QueryInterface("+iid+")\n");
    if(
    	iid.equals(Components.interfaces.dhIProbe) ||
    	iid.equals(Components.interfaces.dhIProbeMouseListener) ||
    	iid.equals(Components.interfaces.nsISupports)
	) {
	    return this;
    }
	throw Components.results.NS_ERROR_NO_INTERFACE;
}

var vMLProbeModule = {
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
        compMgr.registerFactoryLocation(NS_MLPROBE_CID,
                                        "MLProbe",
                                        NS_MLPROBE_PROG_ID, 
                                        fileSpec,
                                        location,
                                        type);
    },

	unregisterSelf: function(compMgr, fileSpec, location) {
    	compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    	compMgr.unregisterFactoryLocation(NS_DH_MLPROBE_CID, fileSpec);
	},

    /*
     * The GetClassObject method is responsible for producing Factory and
     * SingletonFactory objects (the latter are specialized for services).
     */
    getClassObject: function (compMgr, cid, iid) {
        if (!cid.equals(NS_MLPROBE_CID)) {
	    	throw Components.results.NS_ERROR_NO_INTERFACE;
		}

        if (!iid.equals(Components.interfaces.nsIFactory)) {
	    	throw Components.results.NS_ERROR_NOT_IMPLEMENTED;
		}

        return this.vMLProbeFactory;
    },

    /* factory object */
    vMLProbeFactory: {
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
	
	    	if(Util==null) 
	    		Util=Components.classes["@downloadhelper.net/util-service;1"]
					.getService(Components.interfaces.dhIUtilService);

			return new MLProbe().QueryInterface(iid);
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
    return vMLProbeModule;
}

