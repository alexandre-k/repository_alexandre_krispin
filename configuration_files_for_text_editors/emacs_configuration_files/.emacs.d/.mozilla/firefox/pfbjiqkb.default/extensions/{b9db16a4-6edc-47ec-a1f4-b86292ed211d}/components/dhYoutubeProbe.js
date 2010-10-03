/******************************************************************************
 *            Copyright (c) 2006-2009 Michel Gutierrez. All Rights Reserved.
 ******************************************************************************/

/**
 * Constants.
 */

const NS_YTPROBE_CID = Components.ID("{506124c4-0076-48d2-bfee-14bb3187560e}");
const NS_YTPROBE_PROG_ID = "@downloadhelper.net/youtube-probe;1";
const DHNS = "http://downloadhelper.net/1.0#";

var Util=null;

/**
* Object constructor
*/
function YTProbe() {
	try {
		//dump("[YTProbe] constructor\n");
		var prefService=Components.classes["@mozilla.org/preferences-service;1"]
		                                   .getService(Components.interfaces.nsIPrefService);
		this.pref=prefService.getBranch("dwhelper.");
		this.core=Components.classes["@downloadhelper.net/core;1"].
			getService(Components.interfaces.dhICore);
		this.core.registerProbe(this);
	} catch(e) {
		dump("[YTProbe] !!! constructor: "+e+"\n");
	}
}

YTProbe.prototype = {}

YTProbe.prototype.handleDocument=function(document,window) {
	try {
		//dump("[YTProbe] handleDocument("+document.URL+")\n");
		if(/^http:\/\/[^\/]*\.?youtube\.[^\/\.]+/.test(document.URL)) {
			var dom=document.documentElement;
			var scripts=Util.xpGetStrings(dom,".//script/text()",{});
			var videoId=null;
			var t=null;
			for(var i=0;i<scripts.length;i++) {
				var script=scripts[i];
				var match=/\"video_id\": \"(.*?)\".*\"t(?:oken)?\": \"(.*?)\"/m.exec(script);
				if(match!=null && match.length==3) {
					videoId=match[1];
					t=match[2];
					break;
				}
				var match=/\"t(?:oken)?\": \"(.*?)\".*\"video_id\": \"(.*?)\"/m.exec(script);
				if(match!=null && match.length==3) {
					videoId=match[2];
					t=match[1];
					break;
				}
			}
			if(videoId==null || t==null) {
				for(var i=0;i<scripts.length;i++) {
					var script=scripts[i];
					var match=/[^_]video_id=([^&]+)(?:&.*)&t=([^&]+)/m.exec(script);
					if(match!=null && match.length==3) {
						videoId=match[1];
						t=match[2];
						break;
					}
					var match=/[&\?]t=(.*?)(?:&|&.*[^_])video_id=(.*?)(?:&|")/m.exec(script);
					if(match!=null && match.length==3) {
						videoId=match[2];
						t=match[1];
						break;
					}
				}
			}
			if(videoId==null || t==null) {
				var embeds=Util.xpGetStrings(dom,".//embed/@src",{});
				for(var i=0;i<embeds.length;i++) {
					var embed=embeds[i];
					var match=/[^_]video_id=(.*?)&.*t=(.*?)(?:&|")/m.exec(embed);
					if(match!=null && match.length==3) {
						videoId=match[1];
						t=match[2];
						break;
					}
				}
				if(videoId==null || t==null) {
					return;
				}
			}
			var title=Util.xpGetString(dom,"/html/head/meta[@name='title']/@content");
			if(title==null || title.length==0) {
				title=Util.xpGetString(dom,".//h3[@id='playnav-restricted-title']/text()");
			}
			if(title==null || title.length==0) {
				title=Util.xpGetString(dom,".//div[@class='content']/div/a/img[@title]/@title");
			}			
			if(title)
				title=title.replace(/"/g,"");
			var url="http://www.youtube.com/get_video?video_id="+videoId+"&t="+t;

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
			
			var checkHQ=this.pref.getBoolPref("yt-check-hq");
			if(checkHQ) {
				var desc=Components.classes["@mozilla.org/properties;1"].
					createInstance(Components.interfaces.nsIProperties);
				var checker=Components.classes["@downloadhelper.net/ythq-checker;1"].
					createInstance(Components.interfaces.dhIYTHQChecker);
				desc.set("window",window);
				desc.set("document",document);
				Util.setPropsString(desc,"page-url",document.URL);
				Util.setPropsString(desc,"youtube-title",title);
				Util.setPropsString(desc,"label",Util.getText("label.high-quality-prefix")+" "+title);
				Util.setPropsString(desc,"base-name",fileName);
				Util.setPropsString(desc,"file-name",fileName);
				Util.setPropsString(desc,"icon-url","http://www.youtube.com/favicon.ico");
				checker.checkMulti(url,this,desc);
			}
			
			var desc=Components.classes["@mozilla.org/properties;1"].
				createInstance(Components.interfaces.nsIProperties);
			Util.setPropsString(desc,"media-url",url);
			Util.setPropsString(desc,"page-url",document.URL);
			Util.setPropsString(desc,"label",title);
			Util.setPropsString(desc,"base-name",fileName);
			Util.setPropsString(desc,"file-name",fileName+".flv");
			Util.setPropsString(desc,"file-extension","flv");
			Util.setPropsString(desc,"capture-method","youtube");
			Util.setPropsString(desc,"icon-url","http://www.youtube.com/favicon.ico");
			this.core.addEntryForDocument(desc,document,window);
		} 
	} catch(e) {
		dump("!!! [YTProbe] handleDocument("+document.URL+"): "+e+"\n");
	}
}

YTProbe.prototype.checkedYTHQ=function(url,args,format,extension) {
	//dump("[YTProbe] checkedYTHQ("+url+",args)\n");
	if(url) {
		var desc=this.core.cloneEntry(args);
		Util.setPropsString(desc,"media-url",url);
		var document=desc.get("document",Components.interfaces.nsIDOMDocument);
		var window=desc.get("window",Components.interfaces.nsIDOMWindow);
		var fileName=Util.getPropsString(desc,"file-name");
		Util.setPropsString(desc,"file-name",fileName+"."+extension);
		Util.setPropsString(desc,"file-extension",extension);
		Util.setPropsString(desc,"capture-method","youtube-hq");
		var title=Util.getPropsString(desc,"youtube-title");
		var prefix=Util.getFText("label.high-quality-prefix2",[""+format],1)+" ";
		Util.setPropsString(desc,"label-prefix",prefix);
		Util.setPropsString(desc,"label",prefix+title);
		this.core.addEntryForDocument(desc,document,window);
	}
}

YTProbe.prototype.handleRequest=function(request) {
}
	
YTProbe.prototype.handleResponse=function(request) {
}
	
YTProbe.prototype.QueryInterface = function(iid) {
	//dump("[YTProbe] QueryInterface("+iid+")\n");
    if(
    	iid.equals(Components.interfaces.dhIProbe) ||
    	iid.equals(Components.interfaces.dhIYTHQCheckerListener) ||
    	iid.equals(Components.interfaces.nsISupports)
	) {
	    return this;
    }
	throw Components.results.NS_ERROR_NO_INTERFACE;
}

var vYTProbeModule = {
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
        compMgr.registerFactoryLocation(NS_YTPROBE_CID,
                                        "YTProbe",
                                        NS_YTPROBE_PROG_ID, 
                                        fileSpec,
                                        location,
                                        type);
    },

	unregisterSelf: function(compMgr, fileSpec, location) {
    	compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    	compMgr.unregisterFactoryLocation(NS_DH_YTPROBE_CID, fileSpec);
	},

    /*
     * The GetClassObject method is responsible for producing Factory and
     * SingletonFactory objects (the latter are specialized for services).
     */
    getClassObject: function (compMgr, cid, iid) {
        if (!cid.equals(NS_YTPROBE_CID)) {
	    	throw Components.results.NS_ERROR_NO_INTERFACE;
		}

        if (!iid.equals(Components.interfaces.nsIFactory)) {
	    	throw Components.results.NS_ERROR_NOT_IMPLEMENTED;
		}

        return this.vYTProbeFactory;
    },

    /* factory object */
    vYTProbeFactory: {
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

			return new YTProbe().QueryInterface(iid);
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
    return vYTProbeModule;
}

