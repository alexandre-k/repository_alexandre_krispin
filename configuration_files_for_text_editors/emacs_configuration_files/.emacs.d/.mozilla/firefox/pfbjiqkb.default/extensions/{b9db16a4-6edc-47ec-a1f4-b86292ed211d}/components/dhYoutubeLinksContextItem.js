/******************************************************************************
 *            Copyright (c) 2006-2009 Michel Gutierrez. All Rights Reserved.
 ******************************************************************************/

/**
 * Constants.
 */

const NS_YTLCITEM_CID = Components.ID("{fc78d3e8-91f0-4ccd-a830-aefd00595f43}");
const NS_YTLCITEM_PROG_ID = "@downloadhelper.net/youtube-links-context-item;1";
const DHNS = "http://downloadhelper.net/1.0#";

var Util=null;
var Node=null;

/**
* Object constructor
*/
function YTLCItem() {
	try {
		//dump("[YTLCItem] constructor\n");
		this.core=Components.classes["@downloadhelper.net/core;1"].
			getService(Components.interfaces.dhICore);
		var prefService=Components.classes["@mozilla.org/preferences-service;1"]
		                                   .getService(Components.interfaces.nsIPrefService);
		this.pref=prefService.getBranch("dwhelper.");
		this.currentPage=null;
		this.pageQueue=[];
	} catch(e) {
		dump("[YTLCItem] !!! constructor: "+e+"\n");
	}
}

YTLCItem.prototype = {}

YTLCItem.prototype.canHandle=function(document,window,item) {
	//dump("[YTLCItem] canHandle()\n");
	try {
		var popupNode=document.popupNode;
		if(popupNode==null)
			popupNode=this.getPopupNode(window);
		if(popupNode==null)
			return false;
		var links=this.getYTLinks(popupNode);
/*
		dump("=>Found "+links.length+" links\n");
		for(var i in links) {
			dump("  - "+links[i]+"\n");
		}
*/
		if(links.length>0)
			return true;
		return false;
	} catch(e) {
		dump("!!! [YTLCItem] canHandle(): "+e+"\n");
	}
}

YTLCItem.prototype.handle=function(document,window,item) {
	//dump("[YTLCItem] handle()\n");
	try {
		var popupNode=document.popupNode;
		if(popupNode==null)
			popupNode=this.getPopupNode(window);
		if(popupNode==null)
			return false;
		var links=this.getYTLinks(popupNode);
		for(var i=0;i<links.length;i++) {
			this.queuePage(links[i]);
		}
	} catch(e) {
		dump("!!! [YTLCItem] handle(): "+e+"\n");
	}
}

YTLCItem.prototype.cleanLinks=function(links) {
	var links0={};
	for(var i in links) {
		var link=links[i];
		var linkKey=link.replace(/&hd=1/g,'');
		if(links0[linkKey]==null || links0[linkKey].length<link.length)
			links0[linkKey]=link;
	}
	links=[];
	for(var i in links0) {
		links.push(links0[i]);
	}
	return links;
}

YTLCItem.prototype.getYTLinks=function(popupNode) {
	var doc=popupNode.ownerDocument;
	var baseUri=Components.classes["@mozilla.org/network/io-service;1"]
               .getService(Components.interfaces.nsIIOService)
               .newURI(doc.URL, null, null);
	var links=[];
	var seln=doc.defaultView.getSelection();
	if(seln.rangeCount>0) {
		var range=seln.getRangeAt(0);
		if(!range.collapsed) {
			var linksMap={};
			var firstLink=this.getLinkInAncestors(range.startContainer,baseUri);
			if(firstLink!=null)
				linksMap[firstLink]="";
			var ancestor=range.commonAncestorContainer;
			var url0=this.getLinkInAncestors(ancestor, baseUri);
			if(url0!=null) {
				links.push(url0);
			} else {
				this.getLinks(ancestor, linksMap, baseUri, { 
					inRange: false, 
					startContainer: range.startContainer,
					endContainer: range.endContainer
					});
				for(var url in linksMap) {
					links.push(url);
				}
			}
			return this.cleanLinks(links);
		}
	} 
	
	if(popupNode) {
		var url=this.getLinkInAncestors(popupNode, baseUri);
		if(url!=null)
			links.push(url);
	}

	return this.cleanLinks(links);
}

YTLCItem.prototype.getLinks=function(node,linksMap, baseUri, scanData) {
	if(scanData.inRange==false && node==scanData.startContainer)
		scanData.inRange=true;

	if(node.nodeType==Node.ELEMENT_NODE) {
		if(scanData.inRange==true) {
			if(node.tagName.toLowerCase()=="a") {
				var url=this.getURL(baseUri,node);
				if(url)
					linksMap[url]="";
			}
		}
		var node0=node.firstChild;
		while(node0) {
			this.getLinks(node0,linksMap,baseUri,scanData);
			node0=node0.nextSibling;
		}
	}

	if(scanData.inRange==true && node==scanData.endContainer)
		scanData.inRange=false;
}

YTLCItem.prototype.getLinkInAncestors=function(node,baseUri) {
	while(node!=null) {
		if(node.nodeType==Node.ELEMENT_NODE) {
			if(node.tagName.toLowerCase()=="a") {
				var url=this.getURL(baseUri,node);
				if(url)
					return url;
			}
		}
		node=node.parentNode;
	}
	return null;
}

YTLCItem.prototype.getURL=function(baseUri,node) {
	var href=node.getAttribute("href");
	if(href!=null && href.length>0) {
		var url=baseUri.resolve(href);
		if(url.match(/^http\:\/\/(?:[^\/\.]+\.)?youtube\.com\/watch\?/))
			return url;
	}	
	if(node.hasAttribute("onclick")) {
		try {
			var videoId=/onPlayVideos\(\['([a-zA-Z0-9]*)'\]\)/.exec(node.getAttribute("onclick"))[1];
			if(videoId) {
				return "http://www.youtube.com/watch?v="+videoId;
			}
		} catch(e) {}
	}
	return null;
}

YTLCItem.prototype.getPopupNode=function(window) {
	//dump("[YTLCItem] getPopupNode("+window.document.URL+")\n");
	var popupNode=window.document.popupNode;
	return popupNode;
}

YTLCItem.prototype.queuePage=function(url) {
	this.pageQueue.push(url);
	this.checkQueue();
}

YTLCItem.prototype.checkQueue=function() {
	if(this.currentPage!=null)
		return;
	if(this.pageQueue.length>0) {
		this.currentPage=this.pageQueue.shift();
		this.getYTPage(this.currentPage);
	}
}

YTLCItem.prototype.getYTPage=function(url) {
	
	function StreamListener(service,url) {
		this.service=service;
		this.url=url;
	}

	StreamListener.prototype={
			QueryInterface: function(iid) {
			    if (!iid.equals(Components.interfaces.nsISupports) && 
			    	!iid.equals(Components.interfaces.nsIStreamListener)) {
			            throw Components.results.NS_ERROR_NO_INTERFACE;
			        }
			    return this;
			},
			onStartRequest: function(request,context) {
				this.httpChannel=request.QueryInterface(Components.interfaces.nsIHttpChannel);
				this.responseStatus=this.httpChannel.responseStatus;
				this.data="";
			},
			onDataAvailable: function(request,context,inputStream,offset,count) {
				var sstream = Components.classes["@mozilla.org/intl/converter-input-stream;1"]
	                   .createInstance(Components.interfaces.nsIConverterInputStream);
				sstream.init(inputStream, "utf-8", 256, 
					Components.interfaces.nsIConverterInputStream.DEFAULT_REPLACEMENT_CHARACTER);
				var str={};
				var n=sstream.readString(128,str);
				while(n>0) {
					this.data+=str.value;
					str={};
					n=sstream.readString(128,str);
				}
			},
			onStopRequest: function(request,context,nsresult) {
				if(this.responseStatus==200) {
					this.service.loadedPage(this.url,this.data);
				}
				this.service.currentPage=null;
				this.service.checkQueue();
			}
		}

	var ioService = Components.classes["@mozilla.org/network/io-service;1"]
	                                   .getService(Components.interfaces.nsIIOService);
     var uri = ioService.newURI(url, null, null);
     var channel = ioService.newChannelFromURI(uri);
     channel.asyncOpen(new StreamListener(this,url), null);
}

YTLCItem.prototype.loadedPage=function(url,text) {
	dump("[YTLCItem] loadedPage("+url+","+text.length+" bytes)\n");
	try {
		var title=null;
		var videoId=null;
		var tParam=null;
		var pageUrl=url;
		var m=/meta name="title" content="(.*)"/.exec(text);
		if(m!=null && m.length==2)
			title=m[1];
		m=/\"video_id\": \"(.*?)\".*\"t(?:oken)?\": \"(.*?)\"/.exec(text);
		if(m!=null && m.length==3) {
			videoId=m[1];
			tParam=m[2];
		} else {
			m=/\"t(?:oken)?\": \"(.*?)\".*\"video_id\": \"(.*?)\"/.exec(text);
			if(m!=null && m.length==3) {
				videoId=m[2];
				tParam=m[1];
			}
		}
		
		if(videoId==null || tParam==null) {
			var match=/[^_]video_id=([^&]+)(?:&.*)&t=([^&]+)/m.exec(text);
			if(match!=null && match.length==3) {
				videoId=match[1];
				tParam=match[2];
			}
		}

		if(videoId==null || tParam==null) {
			var match=/[&\?]t=(.*?)(?:&|&.*[^_])video_id=(.*?)(?:&|")/m.exec(text);
			if(match!=null && match.length==3) {
				videoId=match[2];
				tParam=match[1];
			}
		}
		
		if(title!=null && videoId!=null & tParam!=null) {
			var url="http://www.youtube.com/get_video?video_id=";
			url+=videoId;
			url+="&t=";
			url+=tParam;
			var orgFileName=title+".flv";
			orgFileName=orgFileName.replace(/[^a-zA-Z0-9\.\-]/g,"_");
			var format=null;
			var fileName=orgFileName;
			
			var desc=Components.classes["@mozilla.org/properties;1"].
				createInstance(Components.interfaces.nsIProperties);
			Util.setPropsString(desc,"media-url",url);
			Util.setPropsString(desc,"page-url",pageUrl);
			Util.setPropsString(desc,"label",title);
			
			var fileName=title;
			var unmodifiedFilename=false;
			try {
				unmodifiedFilename=this.pref.getBoolPref("yt-unmodified-filename");		
			} catch(e) {}
			fileName=fileName.replace(/[\/"\?\*:\|"'\\]/g,"_");
			if(unmodifiedFilename==false) {
				var keepSpaces=false;
				try {
					keepSpaces=this.pref.getBoolPref("yt-keep-spaces");
				} catch(e) {}
				if(keepSpaces)
					fileName=fileName.replace(/[^a-zA-Z0-9\.\- ]/g,"_");
				else
					fileName=fileName.replace(/[^a-zA-Z0-9\.\-]/g,"_");
			}
			Util.setPropsString(desc,"file-name",fileName);
			Util.setPropsString(desc,"icon-url","http://www.youtube.com/favicon.ico");

			var doPreferHQ=true;
			try {
				doPreferHQ=this.pref.getBoolPref("yt-prefer-hq");
			} catch(e) {}
			if(doPreferHQ) {
				var checker=Components.classes["@downloadhelper.net/ythq-checker;1"].
					createInstance(Components.interfaces.dhIYTHQChecker);
				checker.check(url,this,desc);

			} else {
				Util.setPropsString(desc,"file-name",fileName+".flv");
				this.core.quickProcess(desc);
			}
		}
	} catch(e) {
		dump("!!! [YTLCItem] loadedPage("+url+","+text.length+" bytes)\n");
	}
}

YTLCItem.prototype.checkedYTHQ=function(url,desc,format,extension) {
	//dump("[YTLCItem] checkedYTHQ("+url+",args)\n");
	var fileName=Util.getPropsString(desc,"file-name");
	if(url!=null) {
		Util.setPropsString(desc,"file-name",fileName+"."+extension);
		Util.setPropsString(desc,"media-url",url);
	} else {
		Util.setPropsString(desc,"file-name",fileName+".flv");
	}
	this.core.quickProcess(desc);
}

YTLCItem.prototype.QueryInterface = function(iid) {
	//dump("[YTLCItem] QueryInterface("+iid+")\n");
    if(
    	iid.equals(Components.interfaces.dhIContextItem) ||
    	iid.equals(Components.interfaces.dhIYTHQCheckerListener) ||
    	iid.equals(Components.interfaces.nsISupports)
	) {
	    return this;
    }
	throw Components.results.NS_ERROR_NO_INTERFACE;
}

var vYTLCItemModule = {
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
        compMgr.registerFactoryLocation(NS_YTLCITEM_CID,
                                        "YTLCItem",
                                        NS_YTLCITEM_PROG_ID, 
                                        fileSpec,
                                        location,
                                        type);
    },

	unregisterSelf: function(compMgr, fileSpec, location) {
    	compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    	compMgr.unregisterFactoryLocation(NS_DH_YTLCITEM_CID, fileSpec);
	},

    /*
     * The GetClassObject method is responsible for producing Factory and
     * SingletonFactory objects (the latter are specialized for services).
     */
    getClassObject: function (compMgr, cid, iid) {
        if (!cid.equals(NS_YTLCITEM_CID)) {
	    	throw Components.results.NS_ERROR_NO_INTERFACE;
		}

        if (!iid.equals(Components.interfaces.nsIFactory)) {
	    	throw Components.results.NS_ERROR_NOT_IMPLEMENTED;
		}

        return this.vYTLCItemFactory;
    },

    /* factory object */
    vYTLCItemFactory: {
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

			if(Util==null) 
	    		Util=Components.classes["@downloadhelper.net/util-service;1"]
					.getService(Components.interfaces.dhIUtilService);

			return new YTLCItem().QueryInterface(iid);
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
    return vYTLCItemModule;
}

