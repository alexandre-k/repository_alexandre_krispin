/******************************************************************************
 *            Copyright (c) 2006-2009 Michel Gutierrez. All Rights Reserved.
 ******************************************************************************/

/**
 * Constants.
 */

const NS_LICENSE_HANDLER_CID = Components.ID("{b60070dc-d471-4007-ab63-b30626e5ab5c}");
const NS_LICENSE_HANDLER_PROG_ID = "@downloadhelper.net/license-handler;1";
const DHNS = "http://downloadhelper.net/1.0#";

var Util=null;

/**
* Object constructor
*/
function LicenseHandler() {
	//dump("[LicenseHandler] constructor\n");
    var uriLoader = Components.classes["@mozilla.org/uriloader;1"].getService(Components.interfaces.nsIURILoader);
    uriLoader.registerContentListener(this);
}

LicenseHandler.prototype = {
		get loadCookie() { return this.mLoadCookie; },
		set loadCookie(newval) { return this.mLoadCookie=newval; },
		get parentContentListener() { return this.mParentContentListener; },
		set parentContentListener(newval) { return this.mParentContentListener=newval; }
}

LicenseHandler.prototype.canHandleContent = function( 
	contentType, 
	isContentPreferred, 
	desiredContentType )  {

	//dump("[LicenseHandler] canHandleContent contentType: "+contentType+"\n");

	if(contentType=="application/x-downloadhelper-license") 
		return true;
	else
		return false;
	
}

LicenseHandler.prototype.doContent = function( 
	contentType , 
	isContentPreferred , 
	request , 
	contentHandler ) {
	
	//dump("[LicenseHandler] doContent contentType: "+contentType+"\n");

	if(contentType!="application/x-downloadhelper-license")
		return false;
		
	function StreamListener() {
		this.outputStream=null;
		this.debugData=false;
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

			try {

			this.httpChannel=request.QueryInterface(Components.interfaces.nsIHttpChannel);
			this.responseStatus=this.httpChannel.responseStatus;
			this.responseStatusText=this.httpChannel.responseStatusText;
			this.contentType=this.httpChannel.getResponseHeader("content-type");			
			this.data="";
			
			//dump("[LicenseHandler/StreamListener] onStartRequest response: "+
			//	this.responseStatus+" "+this.responseStatusText+"\n");

			} catch(e) {
				dump("[LicenseHandler/StreamListener] onStartRequest error: "+e+"\n");	
			}

		},
		onDataAvailable: function(request,context,inputStream,offset,count) {
			//dump("[LicenseHandler/StreamListener] onDataAvailable\n");	

			try {
			
			var sstream = Components.classes["@mozilla.org/intl/converter-input-stream;1"]
                   .createInstance(Components.interfaces.nsIConverterInputStream);
			sstream.init(inputStream, "utf-8", 256, 
				Components.interfaces.nsIConverterInputStream.DEFAULT_REPLACEMENT_CHARACTER);

			var str={};
			var n=sstream.readString(128,str);
			while(n>0) {
				this.data+=str.value;
				//dump("[LicenseHandler/StreamListener] onDataAvailable read: "+str.value+"\n");	
				str={};
				n=sstream.readString(128,str);
			}

			} catch(e) {
				dump("[LicenseHandler/StreamListener] onDataAvailable error: "+e+"\n");	
			}

		},
		onStopRequest: function(request,context,nsresult) {
			//dump("[LicenseHandler/StreamListener] onStopRequest\n");

			try {

			if(this.responseStatus==200) {

				//dump("[LicenseHandler/StreamListener] parsing data: "+this.data+"\n");			
					
				var parser=Components.classes["@mozilla.org/xmlextras/domparser;1"].
					createInstance(Components.interfaces.nsIDOMParser);
				var doc=parser.parseFromString(this.data,"text/xml");
				if(doc!=null) {
					var licenseKey=Util.xpGetString(doc.documentElement,"/downloadhelper-license/license-key");
					//dump("[LicenseHandler/StreamListener] license: "+licenseKey+"\n");
					if(licenseKey!=null) {
						var convertMgr=Components.classes["@downloadhelper.net/convert-manager-component"]
							.getService(Components.interfaces.dhIConvertMgr);
						convertMgr.register(licenseKey);
					} else {
						dump("[LicenseHandler/StreamListener] no license key found: "+this.data+"\n");
					}
				} else {
					dump("[LicenseHandler/StreamListener] invalid license file: "+this.data+"\n");
				}
			}
			
			} catch(e) {
				dump("[LicenseHandler/StreamListener] onStopRequest error: "+e+"\n");
			}

		}
	}
	
	try {
		contentHandler.value=new StreamListener();
	} catch(e) {
		dump("[LicenseHandler] openAsync error: "+e+"\n");	
	}
		
	return false;
}

LicenseHandler.prototype.isPreferred = function( 
	contentType , 
	desiredContentType ) {

	//dump("[LicenseHandler] isPreferred contentType: "+contentType+"\n");

	if(contentType=="application/x-downloadhelper-license") 
		return true;
	else
		return false;

}


LicenseHandler.prototype.onStartURIOpen = function( URI ) {

	//dump("[LicenseHandler] onStartURIOpen: "+URI.spec+"\n");

	return false;
}

LicenseHandler.prototype.GetWeakReference = function( ) {

	//dump("[LicenseHandler] GetWeakReference\n");

	return this;
}


LicenseHandler.prototype.QueryInterface = function(iid) {
	//dump("[LicenseHandler] QueryInterface("+iid+")\n");
    if(
    	iid.equals(Components.interfaces.nsISupports) ||
    	iid.equals(Components.interfaces.nsIURIContentListener) ||
    	iid.equals(Components.interfaces.nsISupportsWeakReference)
	) {
        return this;
        }
    throw Components.results.NS_ERROR_NO_INTERFACE;
}

var vLicenseHandlerModule = {
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
        compMgr.registerFactoryLocation(NS_LICENSE_HANDLER_CID,
                                        "LicenseHandler",
                                        NS_LICENSE_HANDLER_PROG_ID, 
                                        fileSpec,
                                        location,
                                        type);
    },

	unregisterSelf: function(compMgr, fileSpec, location) {
    	compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    	compMgr.unregisterFactoryLocation(NS_DH_LICENSE_HANDLER_CID, fileSpec);
	},

    /*
     * The GetClassObject method is responsible for producing Factory and
     * SingletonFactory objects (the latter are specialized for services).
     */
    getClassObject: function (compMgr, cid, iid) {
        if (!cid.equals(NS_LICENSE_HANDLER_CID)) {
	    	throw Components.results.NS_ERROR_NO_INTERFACE;
		}

        if (!iid.equals(Components.interfaces.nsIFactory)) {
	    	throw Components.results.NS_ERROR_NOT_IMPLEMENTED;
		}

        return this.vLicenseHandlerFactory;
    },

    /* factory object */
    vLicenseHandlerFactory: {
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

			return new LicenseHandler().QueryInterface(iid);
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
    return vLicenseHandlerModule;
}

