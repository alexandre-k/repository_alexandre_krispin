/******************************************************************************
 *            Copyright (c) 2006-2009 Michel Gutierrez. All Rights Reserved.
 ******************************************************************************/

/**
 * Constants.
 */

const NS_DH_MP3TUNES_CID = Components.ID("{59dd53ca-8989-4e95-bc7a-996e8592886e}");
const NS_DH_MP3TUNES_PROG_ID = "@downloadhelper.net/mp3tunes-manager;1";
const DHNS = "http://downloadhelper.net/1.0#";

var Util=null;
var IOS=null;

/**
* Object constructor
*/
function MP3Tunes() {
	try {
		this.token="5418414535";
		//dump("[MP3Tunes] constructor\n");
		var prefService=Components.classes["@mozilla.org/preferences-service;1"]
		                                   .getService(Components.interfaces.nsIPrefService);
		this.pref=prefService.getBranch("dwhelper.mp3tunes.");
	} catch(e) {
		dump("!!! [MP3Tunes] constructor: "+e+"\n");
	}
}

MP3Tunes.prototype = {}

MP3Tunes.prototype.authenticateHandler = function(args,status,doc,httpStatus,text) {
	//dump("[MP3Tunes] authenticateHandler("+args+","+status+","+doc+","+httpStatus+",<text>)\n<text>="+text+"\n");
	try {
		var errorMessage="Invalid server response";
		if(status && doc!=null) {
			var authStatus=Util.xpGetString(doc.documentElement,"/mp3tunes/status");
			if(authStatus=="1") {
				this.sessionId=Util.xpGetString(doc.documentElement,"/mp3tunes/session_id");
				args.observer.observe(this,"mp3tunes-auth-succeeded","");
				return;
			} else {
				errorMessage=Util.xpGetString(doc.documentElement,"/mp3tunes/errorMessage");
				//dump("[MP3Tunes] authenticate failed: "+errorMessage+"\n");				
			}
		}
		args.observer.observe(this,"mp3tunes-auth-failed",errorMessage);
	} catch(e) {
		dump("!!! [MP3Tunes] authenticate error: "+e+"\n");
		args.observer.observe(this,"mp3tunes-auth","Internal error: "+e);
	}
}

MP3Tunes.prototype.authenticate = function(user,password,observer) {
	try {
	//dump("[MP3Tunes] authenticate("+user+","+password+",...)\n");
	var url="https://shop.mp3tunes.com/api/v1/login?output=xml&username="+encodeURIComponent(user)+
		"&password="+encodeURIComponent(password)+"&partner_token="+encodeURIComponent(this.token);
	//dump("url="+url+"\n");
	var uri = IOS.newURI(url, null, null);
	var channel = IOS.newChannelFromURI(uri);
	var listener = new XMLStreamListener(this,"authenticateHandler",{observer: observer});
	channel.notificationCallbacks = listener;
	channel.asyncOpen(listener, null);

	} catch(e) { dump("!!! [MP3Tunes] authenticate error: "+e+"\n"); };
}

MP3Tunes.prototype.accountStatusHandler2 = function(args,status,doc,httpStatus,text) {
	//dump("[MP3Tunes] accountStatusHandler2("+args+","+status+","+doc+","+httpStatus+",<text>)\n<text>="+text+"\n");
	try {
		var errorMessage="Invalid server response";
		if(status && doc!=null) {
			var lockerType=Util.xpGetString(doc.documentElement,"/mp3tunes/user/lockerType");
			var expired=Util.xpGetString(doc.documentElement,"/mp3tunes/user/expired");
			var accountStatus="free";
			if(/premium/i.test(lockerType) && expired=="0")
				accountStatus="premium";
			this.pref.setCharPref("last-status",accountStatus);
			args.observer.observe(this,"mp3tunes-account-status",accountStatus);
			return;
		}
		this.pref.setCharPref("last-status",errorMessage);
		args.observer.observe(this,"mp3tunes-account-status",errorMessage);
	} catch(e) {
		dump("!!! [MP3Tunes] accountStatus2 error: "+e+"\n");
		var errorMessage="Internal error: "+e;
		this.pref.setCharPref("last-status",errorMessage);
		args.observer.observe(this,"mp3tunes-account-status",errorMessage);
	}
}

MP3Tunes.prototype.accountStatusHandler = function(args,status,doc,httpStatus,text) {
	//dump("[MP3Tunes] accountStatusHandler("+args+","+status+","+doc+","+httpStatus+",<text>)\n<text>="+text+"\n");
	try {
		var errorMessage="Invalid server response";
		if(status && doc!=null) {
			var authStatus=Util.xpGetString(doc.documentElement,"/mp3tunes/status");
			if(authStatus=="1") {
				this.sessionId=Util.xpGetString(doc.documentElement,"/mp3tunes/session_id");
				var url="https://shop.mp3tunes.com/api/v1/accountData?output=xml&sid=" + this.sessionId;
				//dump("url="+url+"\n");
				var uri = IOS.newURI(url, null, null);
				var channel = IOS.newChannelFromURI(uri);
				var listener = new XMLStreamListener(this,"accountStatusHandler2",{observer: args.observer});
				channel.notificationCallbacks = listener;
				channel.asyncOpen(listener, null);
				return;
			} else {
				errorMessage=Util.xpGetString(doc.documentElement,"/mp3tunes/errorMessage");
				//dump("[MP3Tunes] authenticate failed: "+errorMessage+"\n");				
			}
		}
		this.pref.setCharPref("last-status",errorMessage);
		args.observer.observe(this,"mp3tunes-account-status",errorMessage);
	} catch(e) {
		dump("!!! [MP3Tunes] accountStatus error: "+e+"\n");
		var errorMessage="Internal error: "+e;
		this.pref.setCharPref("last-status",errorMessage);
		args.observer.observe(this,"mp3tunes-account-status",errorMessage);
	}
}

MP3Tunes.prototype.accountStatus = function(user,password,observer) {
	try {
	//dump("[MP3Tunes] accountStatus("+user+","+password+",...)\n");
	var url="https://shop.mp3tunes.com/api/v1/login?output=xml&username="+encodeURIComponent(user)+
		"&password="+encodeURIComponent(password)+"&partner_token="+encodeURIComponent(this.token);
	//dump("url="+url+"\n");
	var uri = IOS.newURI(url, null, null);
	var channel = IOS.newChannelFromURI(uri);
	var listener = new XMLStreamListener(this,"accountStatusHandler",{observer: observer});
	channel.notificationCallbacks = listener;
	channel.asyncOpen(listener, null);

	} catch(e) { dump("!!! [MP3Tunes] accountStatus error: "+e+"\n"); };
}

MP3Tunes.prototype.createAccountHandler = function(args,status,doc,httpStatus,text) {
	//dump("[MP3Tunes] createAccountHandler("+args+","+status+","+doc+","+httpStatus+",<text>)\n<text>="+text+"\n");
	try {
		var errorMessage="Invalid server response";
		if(status && doc!=null) {
			var accountStatus=Util.xpGetString(doc.documentElement,"/mp3tunes/status");
			if(accountStatus=="1") {
				this.sessionId=Util.xpGetString(doc.documentElement,"/mp3tunes/session_id");
				args.observer.observe(this,"mp3tunes-create-account-succeeded","");
				return;
			} else {
				errorMessage=Util.xpGetString(doc.documentElement,"/mp3tunes/errorMessage");
				//dump("[MP3Tunes] createAccount failed: "+errorMessage+"\n");				
			}
		}
		args.observer.observe(this,"mp3tunes-create-account-failed",errorMessage);
	} catch(e) {
		dump("!!! [MP3Tunes] createAccount error: "+e+"\n");
		args.observer.observe(this,"mp3tunes-create-account-failed","Internal error: "+e);
	}
}

MP3Tunes.prototype.createAccount = function(user,password,firstname,lastname,observer) {
	try {
	//dump("[MP3Tunes] createAccount("+user+","+password+","+firstname+","+lastname+",...)\n");
	var url="https://shop.mp3tunes.com/api/v1/createAccount?output=xml&email="+encodeURIComponent(user)+
		"&password="+encodeURIComponent(password)+
		"&firstname="+encodeURIComponent(firstname)+
		"&lastname="+encodeURIComponent(lastname)+
		"&partner_token="+encodeURIComponent(this.token);
	//dump("url="+url+"\n");
	var uri = IOS.newURI(url, null, null);
	var channel = IOS.newChannelFromURI(uri);
	var listener = new XMLStreamListener(this,"createAccountHandler",{observer: observer});
	channel.notificationCallbacks = listener;
	channel.asyncOpen(listener, null);

	} catch(e) { dump("!!! [MP3Tunes] createAccount error: "+e+"\n"); };
}

MP3Tunes.prototype.uploadFileHandler = function(args,status,doc,httpStatus,text) {
	//dump("[MP3Tunes] uploadFileHandler("+args+","+status+","+doc+","+httpStatus+",<text>)\n<text>="+text+"\n");
	try {
		args.inputStream.close();
		if(status) {
			args.observer.observe(this,"mp3tunes-upload-file-succeeded","");
			return;
		}
		args.observer.observe(this,"mp3tunes-upload-file-failed","HTTP code "+httpStatus);
	} catch(e) {
		dump("!!! [MP3Tunes] uploadFile error: "+e+"\n");
		args.observer.observe(this,"mp3tunes-upload-file-failed","Internal error: "+e);
	}
}

MP3Tunes.prototype.uploadFile = function(file,properties,observer) {
	try {
		//dump("[MP3Tunes] uploadFile("+file.path+",...)\n");
		//dump("[MP3Tunes] uploadFile(): file exist="+file.exists()+"\n");
		//dump("[MP3Tunes] uploadFile(): file size="+file.fileSize+"\n");

		var fis = Components.classes['@mozilla.org/network/file-input-stream;1'].
	    createInstance(Components.interfaces.nsIFileInputStream);
		fis.init(file,1,0,false);
		var signer = Components.classes["@mozilla.org/security/hash;1"]
		     	                   .createInstance(Components.interfaces.nsICryptoHash);
		signer.init(signer.MD5);
		const PR_UINT32_MAX = 0xffffffff;
		signer.updateFromStream(fis,PR_UINT32_MAX);
		var hash = signer.finish(false);
		function toHexString(charCode)
		{
		  return ("0" + charCode.toString(16)).slice(-2);
		}
		var t=[];
		for(var i in hash) {
			t.push(toHexString(hash.charCodeAt(i)));
		}
		var md5=t.join("");
		//dump("[MP3Tunes] uploadFile: md5="+md5+"\n");
		
		if(properties) {
			Util.setPropsString(properties,"cv-md5",md5);
		}
		
		var url="http://content.mp3tunes.com/storage/lockerPut/"+md5+
			"?sid="+encodeURIComponent(this.sessionId)+
			"&partner_token="+encodeURIComponent(this.token);
		//dump("url="+url+"\n");
		var uri = IOS.newURI(url, null, null);
		var channel = IOS.newChannelFromURI(uri);
		var httpChannel = channel.QueryInterface(Components.interfaces.nsIHttpChannel);
		httpChannel.setRequestHeader("X-Original-Filename",file.leafName,false);
		if(properties) {
			var artistName=Util.getPropsString(properties,"mp3tunes-artist-name");
			if(artistName.length)
				httpChannel.setRequestHeader("X-MP3tunes-Artist-Name",artistName,false);
			var albumTitle=Util.getPropsString(properties,"mp3tunes-album-title");
			if(albumTitle.length)
				httpChannel.setRequestHeader("X-MP3tunes-Album-Title",albumTitle,false);
			var trackTitle=Util.getPropsString(properties,"mp3tunes-track-title");
			if(trackTitle.length)
				httpChannel.setRequestHeader("X-MP3tunes-Track-Title",trackTitle,false);
			var trackNumber=Util.getPropsString(properties,"mp3tunes-track-number");
			if(trackNumber.length)
				httpChannel.setRequestHeader("X-MP3tunes-Track-Number",trackNumber,false);
			var year=Util.getPropsString(properties,"mp3tunes-year");
			if(year.length)
				httpChannel.setRequestHeader("X-MP3tunes-Year",year,false);
		}
		httpChannel.requestMethod = "PUT";
		fis = Components.classes['@mozilla.org/network/file-input-stream;1'].
			createInstance(Components.interfaces.nsIFileInputStream);
		fis.init(file,1,0,false);
		var uploadChannel = channel.QueryInterface(Components.interfaces.nsIUploadChannel);
		uploadChannel.setUploadStream(fis, "application/x-binary", -1);

		var listener = new XMLStreamListener(this,"uploadFileHandler",{observer: observer, inputStream: fis});
		channel.notificationCallbacks = listener;
		channel.asyncOpen(listener, null);

	} catch(e) { dump("!!! [MP3Tunes] uploadFile error: "+e+"\n"); };
}


MP3Tunes.prototype.sendToPhoneHandler = function(args,status,doc,httpStatus,text) {
	//dump("[MP3Tunes] setToPhoneHandler("+args+","+status+","+doc+","+httpStatus+",<text>)\n<text>="+text+"\n");
	try {
		var errorMessage="Invalid server response";
		if(status && doc!=null) {
			var accountStatus=Util.xpGetString(doc.documentElement,"/mp3tunes/status");
			if(accountStatus=="1") {
				this.sessionId=Util.xpGetString(doc.documentElement,"/mp3tunes/session_id");
				args.observer.observe(this,"mp3tunes-send-to-phone-succeeded","");
				return;
			} else {
				errorMessage=Util.xpGetString(doc.documentElement,"/mp3tunes/errorMessage");
				//dump("[MP3Tunes] sendToPhone failed: "+errorMessage+"\n");				
			}
		}
		args.observer.observe(this,"mp3tunes-send-to-phone-failed",errorMessage);
	} catch(e) {
		dump("!!! [MP3Tunes] sendToPhone error: "+e+"\n");
		args.observer.observe(this,"mp3tunes-send-to-phone-failed","Internal error: "+e);
	}
}

MP3Tunes.prototype.sendToPhone = function(key, phonenumber, observer) {
	try {
	//dump("[MP3Tunes] sendToPhone("+key+","+phonenumber+",...)\n");
	var url="https://shop.mp3tunes.com/api/v1/sendSMS?output=xml&sid="+this.sessionId+
		"&key="+encodeURIComponent(key)+
		"&number="+encodeURIComponent(phonenumber.replace(/[^0-9]/g,''))+
		"&prefix=1";
	//dump("url="+url+"\n");
	var uri = IOS.newURI(url, null, null);
	var channel = IOS.newChannelFromURI(uri);
	var listener = new XMLStreamListener(this,"sendToPhoneHandler",{observer: observer});
	channel.notificationCallbacks = listener;
	channel.asyncOpen(listener, null);

	} catch(e) { dump("!!! [MP3Tunes] sendToPhone error: "+e+"\n"); };
}



MP3Tunes.prototype.QueryInterface = function(iid) {
	//dump("[MP3Tunes] QueryInterface("+iid+")\n");
    if(	iid.equals(Components.interfaces.dhIMP3Tunes) ||
    	iid.equals(Components.interfaces.nsISupports)) {
    		return this;
        }
    throw Components.results.NS_ERROR_NO_INTERFACE;
}


var vMP3TunesModule = {
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
        //dump("*** Registering MP3Tunes\n");
        compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
        compMgr.registerFactoryLocation(NS_DH_MP3TUNES_CID,
                                        "MP3Tunes",
                                        NS_DH_MP3TUNES_PROG_ID, 
                                        fileSpec,
                                        location,
                                        type);
        //dump("*** Registered MP3Tunes\n");
    },

	unregisterSelf: function(compMgr, fileSpec, location) {
    	compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    	compMgr.unregisterFactoryLocation(NS_DH_MP3TUNES_CID, fileSpec);
	},

    /*
     * The GetClassObject method is responsible for producing Factory and
     * SingletonFactory objects (the latter are specialized for services).
     */
    getClassObject: function (compMgr, cid, iid) {
        if (!cid.equals(NS_DH_MP3TUNES_CID)) {
	    	throw Components.results.NS_ERROR_NO_INTERFACE;
		}

        if (!iid.equals(Components.interfaces.nsIFactory)) {
	    	throw Components.results.NS_ERROR_NOT_IMPLEMENTED;
		}

        return this.vMP3TunesFactory;
    },

    /* factory object */
    vMP3TunesFactory: {
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
	
			//dump("MP3Tunes: create instance\n");

	    	if(Util==null) 
	    		Util=Components.classes["@downloadhelper.net/util-service;1"]
					.getService(Components.interfaces.dhIUtilService);
	    	if(IOS==null)
	    		IOS= Components.classes["@mozilla.org/network/io-service;1"]
				    .getService(Components.interfaces.nsIIOService);

			return new MP3Tunes().QueryInterface(iid);
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
    return vMP3TunesModule;
}

function XMLStreamListener(service,callback,args) {
	this.service=service;
	this.callback=callback;
	this.args=args;
}

XMLStreamListener.prototype={
	QueryInterface: function(iid) {
	    if (iid.equals(Components.interfaces.nsISupports) || 
	    	iid.equals(Components.interfaces.nsIInterfaceRequestor) ||
	    	iid.equals(Components.interfaces.nsIStreamListener)) {
	    	return this;
	    }
        throw Components.results.NS_ERROR_NO_INTERFACE;
	},
	onStartRequest: function(request,context) {
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
		var responseStatus=request.QueryInterface(Components.interfaces.nsIHttpChannel).responseStatus;
		if(responseStatus==200) {
			var parser=Components.classes["@mozilla.org/xmlextras/domparser;1"].
				createInstance(Components.interfaces.nsIDOMParser);
			var doc=parser.parseFromString(this.data,"text/xml");
			this.service[this.callback](this.args,true,doc,responseStatus,this.data);
		} else {
			this.service[this.callback](this.args,false,null,responseStatus,this.data);
		}
	},
	getInterface: function(iid) {
	    if (iid.equals(Components.interfaces.nsISupports) || 
	    	iid.equals(Components.interfaces.nsIInterfaceRequestor) ||
	    	iid.equals(Components.interfaces.nsIStreamListener)) {
	    	return this;
	    }
	    return null;
	},
}

