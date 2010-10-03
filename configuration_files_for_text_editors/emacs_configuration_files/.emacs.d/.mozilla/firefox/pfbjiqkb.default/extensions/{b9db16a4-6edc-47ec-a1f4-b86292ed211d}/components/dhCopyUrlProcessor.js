/******************************************************************************
 *            Copyright (c) 2006-2009 Michel Gutierrez. All Rights Reserved.
 ******************************************************************************/

/**
 * Constants.
 */

const NS_COPYURL_CID = Components.ID("{93e81622-ce06-410e-bc10-4f3dd7617399}");
const NS_COPYURL_PROG_ID = "@downloadhelper.net/copyurl-processor;1";
const DHNS = "http://downloadhelper.net/1.0#";

var Util=null;

/**
* Object constructor
*/
function CopyUrl() {
	try {
		//dump("[CopyUrl] constructor\n");
		this.core=Components.classes["@downloadhelper.net/core;1"].
			getService(Components.interfaces.dhICore);
		this.core.registerProcessor(this);
	} catch(e) {
		dump("[CopyUrl] !!! constructor: "+e+"\n");
	}
}

CopyUrl.prototype = {
		get name() { return "copyurl"; },
		get provider() { return "DownloadHelper"; },
		get title() { return Util.getText("processor.copyurl.title"); },
		get description() { return Util.getText("processor.copyurl.description"); },
		get enabled() { return true; },
}

CopyUrl.prototype.canHandle=function(desc) {
	//dump("[CopyUrl] canHandle()\n");
	var ch=desc.has("media-url");
	return ch;
}

CopyUrl.prototype.requireDownload=function(desc) {
	//dump("[CopyUrl] requireDownload()\n");
	return false;
}

CopyUrl.prototype.preDownload=function(desc) {
	return true;
}

CopyUrl.prototype.handle=function(desc) {
	//dump("[CopyUrl] handle()\n");
	var mediaUrl=Util.getPropsString(desc,"media-url");
	if(mediaUrl) {
		var str = Components.classes["@mozilla.org/supports-string;1"].
			createInstance(Components.interfaces.nsISupportsString); 
		if (!str) return; 
		str.data = mediaUrl; 
		var trans = Components.classes["@mozilla.org/widget/transferable;1"].
			createInstance(Components.interfaces.nsITransferable);
		if (!trans) return; 
		trans.addDataFlavor("text/unicode"); 
		trans.setTransferData("text/unicode",str,mediaUrl.length * 2); 
		var clipid = Components.interfaces.nsIClipboard; 
		var clip = Components.classes["@mozilla.org/widget/clipboard;1"].
			getService(clipid); 
		if (!clip) return; 
		clip.setData(trans,null,clipid.kGlobalClipboard);
		//dump("[CopyUrl] handle(): to clipboard "+mediaUrl+"\n");
	}
}

CopyUrl.prototype.QueryInterface = function(iid) {
	//dump("[CopyUrl] QueryInterface("+iid+")\n");
    if(
       	iid.equals(Components.interfaces.dhIProcessor) ||
    	iid.equals(Components.interfaces.nsISupports)
	) {
	    return this;
    }
	throw Components.results.NS_ERROR_NO_INTERFACE;
}

var vCopyUrlModule = {
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
        compMgr.registerFactoryLocation(NS_COPYURL_CID,
                                        "CopyUrl",
                                        NS_COPYURL_PROG_ID, 
                                        fileSpec,
                                        location,
                                        type);
    },

	unregisterSelf: function(compMgr, fileSpec, location) {
    	compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    	compMgr.unregisterFactoryLocation(NS_DH_COPYURL_CID, fileSpec);
	},

    /*
     * The GetClassObject method is responsible for producing Factory and
     * SingletonFactory objects (the latter are specialized for services).
     */
    getClassObject: function (compMgr, cid, iid) {
        if (!cid.equals(NS_COPYURL_CID)) {
	    	throw Components.results.NS_ERROR_NO_INTERFACE;
		}

        if (!iid.equals(Components.interfaces.nsIFactory)) {
	    	throw Components.results.NS_ERROR_NOT_IMPLEMENTED;
		}

        return this.vCopyUrlFactory;
    },

    /* factory object */
    vCopyUrlFactory: {
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

			return new CopyUrl().QueryInterface(iid);
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
    return vCopyUrlModule;
}

