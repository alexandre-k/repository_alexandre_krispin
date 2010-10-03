/******************************************************************************
 *            Copyright (c) 2006-2009 Michel Gutierrez. All Rights Reserved.
 ******************************************************************************/

/**
 * Constants.
 */

const NS_NWPROBE_CID = Components.ID("{29eb6720-7684-4b04-bc58-c18f554c6d55}");
const NS_NWPROBE_PROG_ID = "@downloadhelper.net/network-probe;1";
const DHNS = "http://downloadhelper.net/1.0#";

var Util=null;

/**
* Object constructor
*/
function NetProbe() {
	try {
		//dump("[NetProbe] constructor\n");
		var prefService=Components.classes["@mozilla.org/preferences-service;1"]
		                                   .getService(Components.interfaces.nsIPrefService);
		this.pref=prefService.getBranch("dwhelper.");
		
		this.entries={};
		this.updateDone=false;
		this.prefBranch2=this.pref.QueryInterface(Components.interfaces.nsIPrefBranch2);
		this.prefBranch2.addObserver("", this, false);
		this.updateReqExtensions();
		this.updateMediaWeight();
		this.typePattern=new RegExp("^(audio|video)/");
		this.ytSigPattern=/^(http:\/\/(?:[^\/]*youtube\..*|.*origin=[^\/&]*youtube\..*))(?:&|&amp;)signature.*(?:&|&amp;)ip=.*$/;
		this.listMgr=Components.classes["@downloadhelper.net/media-list-manager"]
			                        	.getService(Components.interfaces.dhIMediaListMgr);

		this.cacheService = Components.classes["@mozilla.org/network/cache-service;1"]
		                               		.getService(Components.interfaces.nsICacheService);
		this.httpCacheSession = this.cacheService.createSession("HTTP", 
			Components.interfaces.nsICache.STORE_ANYWHERE,
			Components.interfaces.nsICache.STREAM_BASED);
		this.httpCacheSession.doomEntriesIfExpired=false;
		this.core=Components.classes["@downloadhelper.net/core;1"].
			getService(Components.interfaces.dhICore);
		this.core.registerProbe(this);
	} catch(e) {
		dump("[NetProbe] !!! constructor: "+e+"\n");
	}
}

NetProbe.prototype = {}

NetProbe.prototype.handleDocument=function(document,window) {
}

NetProbe.prototype.handleRequest=function(request) {
	//dump("[NetProbe] handleRequest("+request.name+")\n");
	var url=request.name;
	try {
		var cacheTracker=true;
		try {
			cacheTracker=this.pref.getBoolPref("cache-tracker");
		} catch(e) {}
		if(cacheTracker) {

		    var httpChannel=request.QueryInterface(Components.interfaces.nsIHttpChannel);
			var wnd=null;
			if(httpChannel.notificationCallbacks) {
				try {
					var notif=httpChannel.notificationCallbacks.QueryInterface(Components.interfaces.nsIInterfaceRequestor);
					wnd=notif.getInterface(Components.interfaces.nsIDOMWindow);
				} catch(e) { }
			}
			if(wnd==null && httpChannel.loadGroup && httpChannel.loadGroup.notificationCallbacks) {
				try {
					var lgNotif=httpChannel.loadGroup.notificationCallbacks.QueryInterface(Components.interfaces.nsIInterfaceRequestor);
					wnd=lgNotif.getInterface(Components.interfaces.nsIDOMWindow);
				} catch(e) {}
			}

			/*
			function checkCacheTracker(url,mediaresp,wnd) {
				mediaresp.checkCacheTracker(url,wnd);			
			}
			setTimeout(checkCacheTracker,0,url,this,wnd);
			*/
			this.checkCacheTracker(url,wnd);
		}
			
	} catch(e) { 
		dump("!!! [NetProbe] handleRequest("+request.name+"): "+e+"\n");		
	}

}

NetProbe.prototype.handleResponse=function(request) {
	try {
		
		//dump("[NetProbe] handleResponse("+request.name+")\n");
	
		var murl=request.name;
	    var httpChannel=request.QueryInterface(Components.interfaces.nsIHttpChannel);
	    
		var location=null;
		try {
			location=httpChannel.getResponseHeader("location");
		} catch(e) {}
		if(location) {
			if(this.entries[murl]) {
				delete this.entries[murl];
			}
			return;
		}
	    
		var contentType=null;
		try {
			contentType=httpChannel.getResponseHeader("content-type");
		} catch(e) {}
		var contentLength=null;
		try {
			contentLength=httpChannel.getResponseHeader("content-length");
		} catch(e) {}
		var contentDisp=null;
		try {
			contentDisp=httpChannel.getResponseHeader("content-disposition");
		} catch(e) {}
		
		if(contentLength!=null) {
		
			var tms="100";
			try {
				tms=this.pref.getCharPref("trigger-min-size");
			} catch(e) {}
			tms=parseFloat(tms);
			if(!isNaN(tms)) {
				if(contentLength<tms*1024)
					return;
			}
		}
	
		var wnd=null;
		if(httpChannel.loadGroup && httpChannel.loadGroup.notificationCallbacks) {
			try {
				var lgNotif=httpChannel.loadGroup.notificationCallbacks.QueryInterface(Components.interfaces.nsIInterfaceRequestor);
				wnd=lgNotif.getInterface(Components.interfaces.nsIDOMWindow);
			} catch(e) {}
		}
		if(wnd==null && httpChannel.notificationCallbacks) {
			try {
				var notif=httpChannel.notificationCallbacks.QueryInterface(Components.interfaces.nsIInterfaceRequestor);
				wnd=notif.getInterface(Components.interfaces.nsIDOMWindow);
			} catch(e) {}
		}
	
		var filename=this.analyzeMeta(murl,contentType,contentDisp,contentLength,wnd);
		if(filename!=null) {
	
			var forceCaching=true;
			try {
				forceCaching=this.pref.getBoolPref("force-cache");
			} catch(e) {}
			
			if(forceCaching) {
				httpChannel.setResponseHeader("Cache-Control","max-age="+24*60*60,false);
			}

/*
		    var referer=null;
			try {
				referer=request.getRequestHeader("referer");
			} catch(e) {
			}
*/
		}
	} catch(e) {
		dump("!!! [NetProbe] handleResponse("+request.name+"): "+e+"\n");
	}
}

NetProbe.prototype.checkCacheTracker=function(url,wnd) {
	try {
		var cacheEntryDescriptor=this.httpCacheSession.openCacheEntry(url, 
					Components.interfaces.nsICache.ACCESS_READ, false);
		if(cacheEntryDescriptor && (cacheEntryDescriptor.accessGranted & 1)) {			
				var headers=cacheEntryDescriptor.getMetaDataElement("response-head");
				if(/Location:/i.test(headers)) {
					cacheEntryDescriptor.close();
					return;
				}
	
				var contentType=null;
				try {
					contentType=/Content-Type: *(.*)/i.exec(headers)[1];
				} catch(e) {}
				var contentLength=null;
				try {
					contentLength=/Content-Length: *(.*)/i.exec(headers)[1];
				} catch(e) {}
				var contentDisp=null;
				try {
					contentDisp=/Content-Disposition: *(.*)/i.exec(headers)[1];
				} catch(e) {}
				
				var fn=this.analyzeMeta(url,contentType,contentDisp,contentLength,wnd);
				if(fn!=null) {
					//dump("[MediaResp] checkCacheTracker("+url+"): hit\n");
				}
				cacheEntryDescriptor.close();
			}
	} catch(e) {
		//dump("!!! [NetProbe] checkCacheTracker(): "+e+"\n");
	}		
}

NetProbe.prototype.analyzeMeta = function(murl,contentType,contentDisp,contentLength,wnd) {
    var hit=false;
    var filename=null;
  
	if(contentType!=null && this.typePattern.test(contentType)) {
		var excludeAsf=true;
		try {
			excludeAsf=this.pref.getBoolPref("exclude-ms-asf");
		} catch(e) {}
		if(excludeAsf) {
			if(!/ms-asf$/.test(contentType))
				hit=true;
		} else {
			hit=true;
		}
	}
	
	if(!hit) {
		if(this.reqPattern.test(murl)) {
			hit=true;
		}
	}
	
	if(hit==false && this.mediaWeightEnabled==true) {
		try {
			if(contentLength!=null && isNaN(contentLength)==false && contentLength>=this.mediaWeightThreshold) {
				hit=true;
			}	
		} catch(e) {
		}
	}

	if(hit) {
	
		var extra={};
		filename=this.guessFileName(murl,contentType,contentDisp,wnd,extra);
    
		try {
	
			if(filename.length>64) {
				var parts=/^(.*)(\..*?)$/.exec(filename);
				filename=parts[1].substr(0,64-parts[2].length)+parts[2];
			}
	
			this.entries[murl]={
				url: murl, filename: filename,
				time: new Date().getTime()
				};
	
			var pageUrl=null;
			if(wnd!=null && wnd.document) {
				this.entries[murl].pageUrl=wnd.document.URL;
				pageUrl=wnd.document.URL;
	    	}
			
			var desc=Components.classes["@mozilla.org/properties;1"].
				createInstance(Components.interfaces.nsIProperties);
			Util.setPropsString(desc,"media-url",murl);
			Util.setPropsString(desc,"file-extension",extra.extension);
			Util.setPropsString(desc,"file-name",filename.replace(/[^a-zA-Z0-9\.\- ]/g,"_"));
			Util.setPropsString(desc,"label",filename);
			Util.setPropsString(desc,"page-url",pageUrl);
			Util.setPropsString(desc,"icon-url","chrome://dwhelper/skin/mediaresp.gif");
			Util.setPropsString(desc,"capture-method","network");
	
			if(wnd && wnd.document)
				desc.set("window-document",wnd);
			this.core.addEntry(desc);
	
			try {
				if(/\..{3}$/.test(filename)) {
					var extension=/\.(.{3})$/.exec(filename)[1];
					
					if(extension=="flv" || extension=="mp4") {
						var wnd=null;
						try {
							wnd=httpChannel.notificationCallbacks.getInterface(Components.interfaces.nsIDOMWindow);
						} catch(e) {
						}
						var url=null;
						if(wnd!=null && wnd.document!=null)
							url=wnd.document.URL;
						if(!/^http:\/\/[^\/]*downloadhelper.net\/watch\.php/.test(murl)) {
							this.listMgr.addToList("http://downloadhelper.net/1.0#history-list",
									Util.getPropsString(desc,"media-url"),
									Util.getPropsString(desc,"file-extension"),
									Util.getPropsString(desc,"page-url"),
									Util.getPropsString(desc,"file-name"),
									Util.getPropsString(desc,"page-url"));
						}
					}
				}
			} catch(e) {
			}
			
		
		} catch(e) {
			dump("!!! [NetProbe] analyzeMeta: "+e+"\n");
		}

		return filename;
	}
	return null;
}

NetProbe.prototype.updateReqExtensions=function() {
	var exts="flv|ram|mpg|mpeg|avi|rm|wmv|mov|asf|mp3|rar|movie|divx";
	try {
		exts=this.pref.getCharPref("mediareq-extensions");
	} catch(e) {
	}
	this.reqPattern=new RegExp("[/\\?&]([^/\\?&=]+\\.("+exts+"))(?:$|\\?|&|/)");
}

NetProbe.prototype.updateMediaWeight=function() {
	var mediaWeight=""
	try {
		mediaWeight=this.pref.getCharPref("mediaweight");
	} catch(e) {}
	if(mediaWeight.length==0 || isNaN(parseInt(mediaWeight))) {
		this.mediaWeightEnabled=false;		
	} else {
		this.mediaWeightEnabled=true;		
		this.mediaWeightThreshold=parseInt(mediaWeight)*1024;
	}
}

NetProbe.prototype.observe=function(subject,topic,data) {
	if(topic=="nsPref:changed") {
		if(data=="mediareq-extensions")
			this.updateReqExtensions();
		if(data=="mediaweight")
			this.updateMediaWeight();
	}
}

NetProbe.prototype.guessFileName=function(murl,contentType,contentDisp,wnd,extra) {
	var filename=null;
	var extension=null;
	if(contentDisp!=null) {
		if(/filename=/.test(contentDisp)) {
			filename=/filename="?([^;"]*)/.exec(contentDisp)[1];
			try {
				extension=/.*\.(.*?)$/.exec(filename)[1];
			} catch(e) {
				extension="";
			}
		}
	}
	if(filename==null) {
		if(contentType!=null && /^video\/x-.*$/.test(contentType)) {
			extension=/video\/x-([^;]*)/.exec(contentType)[1];
		} else if(contentType!=null && /^video\/.+$/.test(contentType)) {
			extension=/video\/([^ ,]*).*$/.exec(contentType)[1];
		} else if(contentType!=null && /^audio\/.+$/.test(contentType)) {
			extension=/audio\/(?:x-)?([^ ,]*).*?$/.exec(contentType)[1];
		} else {
			if(/^[^\?]*\.[0-9a-zA-Z]{1,5}$/.test(murl))
				extension=/\.([0-9a-zA-Z]{1,5})$/.exec(murl)[1];
			else
				extension="flv";
		}
		var re=new RegExp("([^/&\\?]+\\."+extension+")(?:$|&|\\?)");
		if(re.test(murl)) {
			var m=re.exec(murl);
			filename=m[1];
		} else if(this.reqPattern.test(murl)) {
			var m=this.reqPattern.exec(murl);
			filename=m[1];
			extension=m[2];
		} else {
			try {
				var title=null;
				if(wnd) {
					title=Util.xpGetString(wnd.document.documentElement,
						"/html/head/meta[@name='title']/@content");
					if(title==null || title=="")
						title=Util.xpGetString(wnd.document.documentElement,
							"/html/head/title");
				}
				if(title==null || title=="")
					title="file-"+Math.floor(Math.random()*1000000000);
				filename=title.replace(/[^a-zA-Z0-9-]+/g,"_")+"."+extension;
			} catch(e) {
				filename="file-"+Math.floor(Math.random()*1000000000)+"."+extension;
			}
		}
	}
	extra.extension=extension;
	return filename;
}

NetProbe.prototype.observe=function(subject,topic,data) {
	//dump("[NetProbe] observe("+subject+","+topic+","+data+")\n");
	if(topic=="quit-application") {
		this.prefBranch2.removeObserver("",this);
		this.observerService.removeObserver(this,"quit-application");
	}
}

NetProbe.prototype.QueryInterface = function(iid) {
	//dump("[NetProbe] QueryInterface("+iid+")\n");
    if(
        	iid.equals(Components.interfaces.dhIProbe) ||
        	iid.equals(Components.interfaces.nsIObserver) ||
    	iid.equals(Components.interfaces.nsISupports)
	) {
	    return this;
    }
	throw Components.results.NS_ERROR_NO_INTERFACE;
}

var vNetProbeModule = {
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
        compMgr.registerFactoryLocation(NS_NWPROBE_CID,
                                        "NetProbe",
                                        NS_NWPROBE_PROG_ID, 
                                        fileSpec,
                                        location,
                                        type);
    },

	unregisterSelf: function(compMgr, fileSpec, location) {
    	compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    	compMgr.unregisterFactoryLocation(NS_DH_NWPROBE_CID, fileSpec);
	},

    /*
     * The GetClassObject method is responsible for producing Factory and
     * SingletonFactory objects (the latter are specialized for services).
     */
    getClassObject: function (compMgr, cid, iid) {
        if (!cid.equals(NS_NWPROBE_CID)) {
	    	throw Components.results.NS_ERROR_NO_INTERFACE;
		}

        if (!iid.equals(Components.interfaces.nsIFactory)) {
	    	throw Components.results.NS_ERROR_NOT_IMPLEMENTED;
		}

        return this.vNetProbeFactory;
    },

    /* factory object */
    vNetProbeFactory: {
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

			return new NetProbe().QueryInterface(iid);
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
    return vNetProbeModule;
}

