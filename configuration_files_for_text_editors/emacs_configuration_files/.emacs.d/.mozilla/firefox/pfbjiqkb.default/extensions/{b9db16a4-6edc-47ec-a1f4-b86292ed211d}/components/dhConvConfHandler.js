/******************************************************************************
 *            Copyright (c) 2008-2009 Michel Gutierrez. All Rights Reserved.
 ******************************************************************************/

/**
 * Constants.
 */

const NS_CONVCONF_HANDLER_CID = Components.ID("{2b554319-e7fb-420e-8f12-1bf88aa2848d}");
const NS_CONVCONF_HANDLER_PROG_ID = "@downloadhelper.net/convconf-handler;1";
const DHNS = "http://downloadhelper.net/1.0#";

var Util=null;

/**
* Object constructor
*/
function ConvConfHandler() {
	this.localstore=Components.classes["@mozilla.org/rdf/datasource;1?name=local-store"]
	                                   .getService(Components.interfaces.nsIRDFDataSource);
	this.promptService=Components.classes["@mozilla.org/embedcomp/prompt-service;1"]
	                          			.getService(Components.interfaces.nsIPromptService);
    var uriLoader = Components.classes["@mozilla.org/uriloader;1"].getService(Components.interfaces.nsIURILoader);
    uriLoader.registerContentListener(this);
}

ConvConfHandler.prototype = {
		get loadCookie() { return this.mLoadCookie; },
		set loadCookie(newval) { return this.mLoadCookie=newval; },
		get parentContentListener() { return this.mParentContentListener; },
		set parentContentListener(newval) { return this.mParentContentListener=newval; }
}

ConvConfHandler.prototype.canHandleContent = function( 
	contentType, 
	isContentPreferred, 
	desiredContentType )  {

	//dump("[ConvConfHandler] canHandleContent contentType: "+contentType+"\n");

	if(contentType=="application/x-downloadhelper-convconf") 
		return true;
	else
		return false;
	
}

ConvConfHandler.prototype.doContent = function( 
	contentType , 
	isContentPreferred , 
	request , 
	contentHandler ) {
	
	//dump("[ConvConfHandler] doContent contentType: "+contentType+"\n");

	if(contentType!="application/x-downloadhelper-convconf")
		return false;
		
	function StreamListener(service) {
		this.outputStream=null;
		this.debugData=false;
		this.service=service;
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
			
			//dump("[ConvConfHandler/StreamListener] onStartRequest response: "+
			//	this.responseStatus+" "+this.responseStatusText+"\n");

			} catch(e) {
				dump("[ConvConfHandler/StreamListener] onStartRequest error: "+e+"\n");	
			}

		},
		onDataAvailable: function(request,context,inputStream,offset,count) {
			//dump("[ConvConfHandler/StreamListener] onDataAvailable\n");	

			try {
			
			var sstream = Components.classes["@mozilla.org/intl/converter-input-stream;1"]
                   .createInstance(Components.interfaces.nsIConverterInputStream);
			sstream.init(inputStream, "utf-8", 256, 
				Components.interfaces.nsIConverterInputStream.DEFAULT_REPLACEMENT_CHARACTER);

			var str={};
			var n=sstream.readString(128,str);
			while(n>0) {
				this.data+=str.value;
				//dump("[ConvConfHandler/StreamListener] onDataAvailable read: "+str.value+"\n");	
				str={};
				n=sstream.readString(128,str);
			}

			} catch(e) {
				dump("[ConvConfHandler/StreamListener] onDataAvailable error: "+e+"\n");	
			}

		},
		onStopRequest: function(request,context,nsresult) {
			//dump("[ConvConfHandler/StreamListener] onStopRequest\n");

			try {

			if(this.responseStatus==200) {

				//dump("[ConvConfHandler/StreamListener] parsing data: "+this.data+"\n");			
					
				var parser=Components.classes["@mozilla.org/xmlextras/domparser;1"].
					createInstance(Components.interfaces.nsIDOMParser);
				var doc=parser.parseFromString(this.data,"text/xml");
				if(doc!=null) {
					var confNodes=Util.xpGetNodes(doc.documentElement,"/conversion-configs/conversion-config",{});
					for(var i=0;i<confNodes.length;i++) {
						var confNode=confNodes[i];
						var confValue=Util.xpGetString(confNode,"value/text()");
						var confLabel=Util.xpGetString(confNode,"label/text()");
						//dump("=>"+confLabel+" = "+confValue+"\n");
						var gotConf=false;
						var confs=Util.getChildResourcesS(this.service.localstore,DHNS+"conv-confs",{});
						for(var i=0;i<confs.length;i++) {
							var confValue0=Util.getPropertyValueRS(this.service.localstore,confs[i],DHNS+"value");
							if(confValue0==confValue) {
								gotConf=true;
								var confLabel0=Util.getPropertyValueRS(this.service.localstore,confs[i],DHNS+"label");
								if(confLabel0==confLabel) {
									this.service.promptService.alert(null,Util.getText("title.conv-conf-handler"),
											Util.getFText("message.conv-conf.already-exist",[confLabel],1));
								} else {
									var r=this.service.promptService.confirm(null,Util.getText("title.conv-conf-handler"),
											Util.getFText("message.conv-conf.rename",[confLabel,confLabel0],2));
									if(r) {
										Util.setPropertyValueRS(this.service.localstore,confs[i],DHNS+"label",confLabel);
									}
								}
							}
						}
						if(gotConf==false) {
							var r=this.service.promptService.confirm(null,Util.getText("title.conv-conf-handler"),
									Util.getFText("message.conv-conf.create",[confLabel],1));
							if(r) {
								var conf=Util.createNodeSR(this.service.localstore,DHNS+"conv-confs",null);
								Util.setPropertyValueRS(this.service.localstore,conf,DHNS+"label",confLabel);
								Util.setPropertyValueRS(this.service.localstore,conf,DHNS+"value",confValue);
							}
						}
					}
				} else {
					dump("[ConvConfHandler/StreamListener] invalid convconf file: "+this.data+"\n");
				}
			}
			
			} catch(e) {
				dump("[ConvConfHandler/StreamListener] onStopRequest error: "+e+"\n");
			}

		}
	}
	
	try {
		contentHandler.value=new StreamListener(this);
	} catch(e) {
		dump("[ConvConfHandler] openAsync error: "+e+"\n");	
	}
		
	return false;
}

ConvConfHandler.prototype.isPreferred = function( 
	contentType , 
	desiredContentType ) {

	dump("[ConvConfHandler] isPreferred contentType: "+contentType+"\n");

	if(contentType=="application/x-downloadhelper-convconf") 
		return true;
	else
		return false;

}


ConvConfHandler.prototype.onStartURIOpen = function( URI ) {

	//dump("[ConvConfHandler] onStartURIOpen: "+URI.spec+"\n");

	return false;
}

ConvConfHandler.prototype.GetWeakReference = function( ) {

	//dump("[ConvConfHandler] GetWeakReference\n");

	return this;
}


ConvConfHandler.prototype.QueryInterface = function(iid) {
	//dump("[ConvConfHandler] QueryInterface("+iid+")\n");
    if(
    	iid.equals(Components.interfaces.nsISupports)==false &&
    	!iid.equals(Components.interfaces.nsIURIContentListener) &&
    	!iid.equals(Components.interfaces.nsISupportsWeakReference)
	) {
            throw Components.results.NS_ERROR_NO_INTERFACE;
        }
    return this;
}

var vConvConfHandlerModule = {
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
        compMgr.registerFactoryLocation(NS_CONVCONF_HANDLER_CID,
                                        "ConvConfHandler",
                                        NS_CONVCONF_HANDLER_PROG_ID, 
                                        fileSpec,
                                        location,
                                        type);
    },

	unregisterSelf: function(compMgr, fileSpec, location) {
    	compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    	compMgr.unregisterFactoryLocation(NS_DH_CONVCONF_HANDLER_CID, fileSpec);
	},

    /*
     * The GetClassObject method is responsible for producing Factory and
     * SingletonFactory objects (the latter are specialized for services).
     */
    getClassObject: function (compMgr, cid, iid) {
        if (!cid.equals(NS_CONVCONF_HANDLER_CID)) {
	    	throw Components.results.NS_ERROR_NO_INTERFACE;
		}

        if (!iid.equals(Components.interfaces.nsIFactory)) {
	    	throw Components.results.NS_ERROR_NOT_IMPLEMENTED;
		}

        return this.vConvConfHandlerFactory;
    },

    /* factory object */
    vConvConfHandlerFactory: {
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

			return new ConvConfHandler().QueryInterface(iid);
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
    return vConvConfHandlerModule;
}

