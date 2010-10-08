/******************************************************************************
 *            Copyright (c) 2006-2009 Michel Gutierrez. All Rights Reserved.
 ******************************************************************************/

/**
 * Constants.
 */

const NS_YTHQCHECKER_CID = Components.ID("{4d140074-831e-456d-bb58-1c29ab1924b4}");
const NS_YTHQCHECKER_PROG_ID = "@downloadhelper.net/ythq-checker;1";
const DHNS = "http://downloadhelper.net/1.0#";

var Util=null;

/**
* Object constructor
*/
function YTHQChecker() {
	try {
		//dump("[YTHQChecker] constructor\n");
		this.listMgr=Components.classes["@downloadhelper.net/media-list-manager"]
			                        	.getService(Components.interfaces.dhIMediaListMgr);
		var prefService=Components.classes["@mozilla.org/preferences-service;1"]
		                                   .getService(Components.interfaces.nsIPrefService);
		this.pref=prefService.getBranch("dwhelper.");
		this.updateFormats();
		this.ios = Components.classes["@mozilla.org/network/io-service;1"]
			                             .getService(Components.interfaces.nsIIOService);
		this.formatIndex=0;
		this.done=false;
	} catch(e) {
		dump("[YTHQChecker] !!! constructor: "+e+"\n");
	}
}

YTHQChecker.prototype = {}

YTHQChecker.prototype.check=function(url,listener,args) {
	//dump("[YTHQChecker] check("+url+")\n");
	this.url=url;
	this.listener=listener;
	this.args=args;
	this.checkNext();
}

function YTHQRequestObserver(client) {
	this.client=client;
}

YTHQRequestObserver.prototype={

	onStartRequest: function(request,context) {
	},

	onStopRequest: function(request,context) {
		if(request.status==0) {
			try {
				//dump("Got format "+this.client.formats[this.client.formatIndex]+"\n");
				var format=this.client.formats[this.client.formatIndex];
				this.client.listener.checkedYTHQ(this.client.url+"&fmt="+format,this.client.args,format,this.client.getExtension(format));
			} catch(e) {
				dump("!!! [YTHQChecker] check callback failed: "+e+"\n");
			}
		} else {
			this.client.formatIndex++;
			this.client.checkNext();
		}
	}
}

YTHQChecker.prototype.checkNext = function() {
	if(this.formatIndex<this.formats.length) {
		var uri=this.ios.newURI(this.url+"&fmt="+this.formats[this.formatIndex],null,null);
		this.listMgr.addCurrentURL(uri.spec);
		this.checker=Components.classes['@mozilla.org/network/urichecker;1'].createInstance(Components.interfaces.nsIURIChecker);
		this.checker.init(uri);
		this.checker.loadFlags = 
			Components.interfaces.nsIRequest.LOAD_BYPASS_CACHE |
			Components.interfaces.nsIRequest.VALIDATE_ALWAYS |
			Components.interfaces.nsIRequest.LOAD_ANONYMOUS;
		//dump("Testing format "+this.formats[this.formatIndex]+"\n");
		this.checker.asyncCheck(new YTHQRequestObserver(this),this);
	} else {
		//dump("Give up checking hq\n");
		try {
			this.listener.checkedYTHQ(null,this.args,0,null);
		} catch(e) {
			dump("!!! [YTHQChecker] check callback failed: "+e+"\n");
		}
	}
}

function YTHQMultiRequestObserver(client,format) {
	this.client=client;
	this.format=format;
}

YTHQMultiRequestObserver.prototype={

	onStartRequest: function(request,context) {
	},
	onStopRequest: function(request,context) {
		if(request.status==0) {
			try {
				//dump("Multi Got format "+this.format+"\n");
				this.client.listener.checkedYTHQ(this.client.url+"&fmt="+this.format,this.client.args,this.format,this.client.getExtension(this.format));
			} catch(e) {
				dump("!!! [YTHQChecker] check callback failed: "+e+"\n");
			}
		}
	}
}

YTHQChecker.prototype.checkMulti=function(url,listener,args) {
	//dump("[YTHQChecker] checkMulti("+url+")\n");
	try {
	this.url=url;
	this.listener=listener;
	this.args=args;	
	this.updateFormats();
	for(var i in this.formats) {
		var format=this.formats[i];
		var uri=this.ios.newURI(this.url+"&fmt="+format,null,null);
		this.listMgr.addCurrentURL(uri.spec);
		this.checker=Components.classes['@mozilla.org/network/urichecker;1'].createInstance(Components.interfaces.nsIURIChecker);
		this.checker.init(uri);
		this.checker.loadFlags = Components.interfaces.nsIRequest.LOAD_BYPASS_CACHE |
			Components.interfaces.nsIRequest.VALIDATE_ALWAYS |
			Components.interfaces.nsIRequest.LOAD_ANONYMOUS;
		//dump("Multi Testing format "+format+"\n");
		this.checker.asyncCheck(new YTHQMultiRequestObserver(this,format),this);
	}
	} catch(e) { dump(e); }
}

YTHQChecker.prototype.getExtension=function(format) {
	if(format==13 || format==17)
		return "3gp";
	else if(format==18 || format==22 || format==37)
		return "mp4";
	else
		return "flv";
}

YTHQChecker.prototype.updateFormats=function() {
	var formats=this.pref.getCharPref("ythq-formats").split(",");
	this.formats=[];
	for(var i in formats) {
		if(formats[i].length>0) {
			this.formats.push(parseInt(formats[i]));
		}
	}
}

YTHQChecker.prototype.QueryInterface = function(iid) {
	//dump("[YTHQChecker] QueryInterface("+iid+")\n");
    if(
    	iid.equals(Components.interfaces.dhIYTHQChecker) ||
    	iid.equals(Components.interfaces.nsIStreamListener) ||
    	iid.equals(Components.interfaces.nsISupports)
	) {
	    return this;
    }
	throw Components.results.NS_ERROR_NO_INTERFACE;
}

var vYTHQCheckerModule = {
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
        compMgr.registerFactoryLocation(NS_YTHQCHECKER_CID,
                                        "YTHQChecker",
                                        NS_YTHQCHECKER_PROG_ID, 
                                        fileSpec,
                                        location,
                                        type);
    },

	unregisterSelf: function(compMgr, fileSpec, location) {
    	compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    	compMgr.unregisterFactoryLocation(NS_DH_YTHQCHECKER_CID, fileSpec);
	},

    /*
     * The GetClassObject method is responsible for producing Factory and
     * SingletonFactory objects (the latter are specialized for services).
     */
    getClassObject: function (compMgr, cid, iid) {
        if (!cid.equals(NS_YTHQCHECKER_CID)) {
	    	throw Components.results.NS_ERROR_NO_INTERFACE;
		}

        if (!iid.equals(Components.interfaces.nsIFactory)) {
	    	throw Components.results.NS_ERROR_NOT_IMPLEMENTED;
		}

        return this.vYTHQCheckerFactory;
    },

    /* factory object */
    vYTHQCheckerFactory: {
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

			return new YTHQChecker().QueryInterface(iid);
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
    return vYTHQCheckerModule;
}

