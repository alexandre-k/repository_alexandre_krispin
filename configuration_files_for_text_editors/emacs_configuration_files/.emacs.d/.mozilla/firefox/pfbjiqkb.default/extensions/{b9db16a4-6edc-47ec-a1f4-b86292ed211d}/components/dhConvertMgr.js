/******************************************************************************
 *            Copyright (c) 2006-2009 Michel Gutierrez. All Rights Reserved.
 ******************************************************************************/

/**
 * Constants.
 */

const NS_CONVERT_MGR_CID = Components.ID("{5f4589e7-114b-4a4b-a63e-06ca7f22439d}");
const NS_CONVERT_MGR_PROG_ID = "@downloadhelper.net/convert-manager-component";
const DHNS = "http://downloadhelper.net/1.0#";

const CONV_METHOD_NONE=Components.interfaces.dhIConvertMgr.CONV_METHOD_NONE;
const CONV_METHOD_UNIX=Components.interfaces.dhIConvertMgr.CONV_METHOD_UNIX;
const CONV_METHOD_WIN_DH=Components.interfaces.dhIConvertMgr.CONV_METHOD_WIN_DH;

var Util=null;

/**
* Object constructor
*/
function ConvertMgr() {
	try {

		this.convRulesFile = Components.classes["@mozilla.org/file/directory_service;1"]
    		.getService(Components.interfaces.nsIProperties)
        	.get("ProfD", Components.interfaces.nsIFile);
        this.convRulesFile.append("dh-conv-rules.rdf");
		if(this.convRulesFile.exists()) {
			this.datasource=Util.getDatasourceFromRDFFile(this.convRulesFile);
		} else {
			var datasource=Components.classes
	      		['@mozilla.org/rdf/datasource;1?name=in-memory-datasource'].
	          		createInstance(Components.interfaces.nsIRDFDataSource);
			this.makeEmptyDataSource(datasource);
			this.setDataSource(datasource);
		}

		var prefService=Components.classes["@mozilla.org/preferences-service;1"]
			.getService(Components.interfaces.nsIPrefService);
		this.pref=prefService.getBranch("dwhelper.");
		
		this.promptService=Components.classes["@mozilla.org/embedcomp/prompt-service;1"]
			.getService(Components.interfaces.nsIPromptService);
		
		this.localstore=Components.classes["@mozilla.org/rdf/datasource;1?name=local-store"]
		                                   .getService(Components.interfaces.nsIRDFDataSource);
		this.RDF = Components.classes["@mozilla.org/rdf/rdf-service;1"].getService().
			QueryInterface(Components.interfaces.nsIRDFService);

		this.delayQueue=[];
		
		this.queueDatasource=Components.classes
      		['@mozilla.org/rdf/datasource;1?name=in-memory-datasource'].
          		createInstance(Components.interfaces.nsIRDFDataSource);

		this.updateUnregistered();
		
		this.currentEntry=null;
		
		if(Util.getPropertyValueSS(this.localstore,DHNS+"conv-confs",DHNS+"conf-inited")==null) {
			Util.setPropertyValueSS(this.localstore,DHNS+"conv-confs",DHNS+"conf-inited","true");
			this.setDefaultConfigs();
		}

	} catch(e) {
		dump("[ConvertMgr] !!! constructor: "+e+"\n");
	}
}

ConvertMgr.prototype = {}

ConvertMgr.prototype.convert=function(sourceFile,targetFile,format,autoClear,listener,entry,ctx) {
	//dump("[ConvertMgr] convert("+sourceFile.path+","+targetFile+","+format+")\n");
	
	try {

	var m=/^(.*?)\/(.*)$/.exec(format);
	var extension=m[1];
	var params=m[2];
	
	if(targetFile==null) {
		targetFile=sourceFile.parent;
		targetFile.append(/^(.*\.)(.*?)$/i.exec(sourceFile.leafName)[1]+extension);
	}
	
	var convRes=Util.createNodeSR(this.queueDatasource,"urn:root",null);
	Util.setPropertyValueRS(this.queueDatasource,convRes,DHNS+"Status","2");
	Util.setPropertyValueRS(this.queueDatasource,convRes,DHNS+"InputFilePath",sourceFile.path);
	Util.setPropertyValueRS(this.queueDatasource,convRes,DHNS+"OutputFilePath",targetFile.path);
	Util.setPropertyValueRS(this.queueDatasource,convRes,DHNS+"InputFilePathShort",sourceFile.leafName);
	Util.setPropertyValueRS(this.queueDatasource,convRes,DHNS+"OutputFilePathShort",targetFile.leafName);
	Util.setPropertyValueRS(this.queueDatasource,convRes,DHNS+"CreationDate",""+new Date());

	var convMethod=this.getConvMethod();
	
	switch(convMethod) {
		case CONV_METHOD_UNIX:
			this.convertUnix(sourceFile,targetFile,params,extension,convRes,autoClear,listener,entry,ctx);
			break;
		case CONV_METHOD_WIN_DH:
			try {
			if(this.checkConverterVersion()==false)
				return;
			} catch(e) {
				dump("!!! checkConverterVersion: "+e+"\n");
				return;
			}
			this.convertDH(sourceFile,targetFile,params,extension,convRes,autoClear,listener,entry,ctx);
			break;
	}
	
	} catch(e) {
		dump("!!! [ConvertMgr] convert: "+e+"\n");
	}                
}

ConvertMgr.prototype.setFFMPEGArgs=function(dEntry,params,sourceFile,targetFile,doTrace,doVhook) {
	var pArgs=["args"];
	var pParams=[params];
	var passes=1;
	if(/.*\/.*/.test(params)) {
		pArgs.push("args2");
		var m=/(.*)\/(.*)/.exec(params);
		passes=2;
		pParams=[m[1],m[2]];
	}
	for(var i=0;i<passes;i++) {
		dEntry[pArgs[i]] = [ "-i", sourceFile.path,"-y","-v","0"];
		dEntry[pArgs[i]]=dEntry[pArgs[i]].concat(pParams[i].split(" "));
		if(doTrace) {
			dEntry[pArgs[i]].push("-Xhello");
			var convertTrace=false;
			try {
				convertTrace=this.pref.getBoolPref("convert-helper.trace");
			} catch(e) {}
			if(convertTrace) {
				dEntry[pArgs[i]].push("-Xloglevel");
				var convertShowArgs=false;
				try {
					convertShowArgs=this.pref.getBoolPref("convert-helper.show-args");
				} catch(e) {}
				if(convertShowArgs)
					dEntry[pArgs[i]].push("2");
				else
					dEntry[pArgs[i]].push("1");
				var logFile=Util.getProfileDir();
				logFile.append("cvhelper.log");
				dEntry[pArgs[i]].push("-Xlogfile");
				dEntry[pArgs[i]].push(logFile.path);
			}
		}
		if(doVhook) {
			var wmFile=Util.getExtensionDir("{b9db16a4-6edc-47ec-a1f4-b86292ed211d}");
			wmFile.append("local");
			wmFile.append("wm.png");

			dEntry[pArgs[i]].push("-Xfn83");
			dEntry[pArgs[i]].push(wmFile.path);
			dEntry[pArgs[i]].push("-vhook");
			dEntry[pArgs[i]].push("vhook/imlib2.dll -x 0 -y 0 -i *");
		}
		dEntry[pArgs[i]].push(targetFile.path);
	}

	/*
	dump("ffmpeg params="+dEntry.args.join(" ")+"\n");
	if(dEntry.args2)
		dump("ffmpeg params (pass#2)="+dEntry.args2.join(" ")+"\n");
	*/
}

ConvertMgr.prototype.convertUnix=function(sourceFile,targetFile,params,extension,convRes,autoClear,listener,entry,ctx) {
	var ffmpegFile = Components.classes["@mozilla.org/file/local;1"]
    	.createInstance(Components.interfaces.nsILocalFile);

	var ffmpegPath="/usr/bin/ffmpeg";
	try {
		ffmpegPath=this.pref.getCharPref("converter-path-ffmpeg");
	} catch(e) {
	}
	try {
		ffmpegFile.initWithPath(ffmpegPath);
		if(!ffmpegFile.exists()) {
			dump("!!![ConvertMgr] convert(): no ffmpeg found\n");
			ffmpegFile=null;
		}
	} catch(e) {
		dump("!!![ConvertMgr] convert(): invalid ffmpeg path\n");
		ffmpegFile=null;
	}
	var mencoderFile = Components.classes["@mozilla.org/file/local;1"]
    	.createInstance(Components.interfaces.nsILocalFile);
	var mencoderPath="/usr/bin/mencoder";
	try {
		mencoderPath=this.pref.getCharPref("converter-path-mencoder");
	} catch(e) {
	}
	try {
		mencoderFile.initWithPath(mencoderPath);
		if(!mencoderFile.exists()) {
			dump("!!![ConvertMgr] convert(): no mencoder found\n");
			mencoderFile=null;
		}
	} catch(e) {
		dump("!!![ConvertMgr] convert(): invalid mencoder path\n");
		mencoderFile=null;
	}
	
	if(mencoderFile==null && ffmpegFile==null) {
		dump("!!![ConvertMgr] convert(): no converter found\n");
	}

	var converterFile=null;
	if(mencoderFile==null && ffmpegFile!=null) {
		converterFile=ffmpegFile;
	} else if(mencoderFile!=null && ffmpegFile==null) {
		converterFile=mencoderFile;
	} else {
		var preferred="ffmpeg";
		try {
			preferred=this.pref.getCharPref("preferred-converter");
		} catch(e) {
		}
		
		if(preferred=="ffmpeg")
			converterFile=ffmpegFile;
		else
			converterFile=mencoderFile;
	}
	
	var dEntry={
		file: converterFile,
		qRes: convRes,
		autoClear: autoClear,
		sourceFile: sourceFile,
		targetFile: targetFile,
		convConf: extension+"/"+params,
		listener: listener,
		entry: entry,
		ctx: ctx
	}
	
	if(converterFile==ffmpegFile) {
		this.setFFMPEGArgs(dEntry,params,sourceFile,targetFile,false,false);
	}
	if(converterFile==mencoderFile) {
		dEntry.args = [ sourceFile.path ];
		// adjust depending on bitrate
		dEntry.args=dEntry.args.concat(["-oac", "copy", "-ovc", "copy"]);			
		dEntry.args=dEntry.args.concat(["-o", targetFile.path]);
	}
	
	this.delayQueue.push(dEntry);
	this.checkConvert();
}

ConvertMgr.prototype.escapePath=function(path) {
	path=path.replace(/\\/g,"\\\\");
	path=path.replace(/ /g,"\\ ");
	path=path.replace(/"/g,"\\\"");
	return path;
}

ConvertMgr.prototype.convertDH=function(sourceFile,targetFile,params,extension,convRes,autoClear,listener,entry,ctx) {

	try {

	var file=this.getInstallDir();
	file.append("cvhelper.exe");

	var unreg=this.updateUnregistered();
	
	var dEntry={
		file: file,
		qRes: convRes,
		autoClear: autoClear,
		sourceFile: sourceFile,
		targetFile: targetFile,
		convConf: extension+"/"+params,
		listener: listener,
		entry: entry,
		ctx: ctx
	}

	this.setFFMPEGArgs(dEntry,params,sourceFile,targetFile,true,unreg);

	this.delayQueue.push(dEntry);
	this.checkConvert();
	
	} catch(e) {
		dump("!!! [ConvertMgr] convertDH: "+e+"\n");
	}
}

ConvertMgr.prototype.execConvert=function(dEntry) {
	this.currentEntry=dEntry;
	Util.setPropertyValueRS(this.queueDatasource,dEntry.qRes,DHNS+"Status","3");
	Util.setPropertyValueRS(this.queueDatasource,dEntry.qRes,DHNS+"ProgressMode","undetermined");
	var Processor=function(convMgr,dEntry) {
		try {
			this.dEntry=dEntry;
			var proxyMgr = Components. classes["@mozilla.org/xpcomproxy;1"].getService(Components.interfaces.nsIProxyObjectManager);
			var proxyFlags = Components.interfaces.nsIProxyObjectManager.FORCE_PROXY_CREATION |
				Components.interfaces.nsIProxyObjectManager.INVOKE_SYNC;
			this.convMgr=proxyMgr.getProxyForObject(null/*uiQueue*/,Components.interfaces.dhIConvertMgr, convMgr, proxyFlags );
		} catch(e) {
			dump("!!![ConvertMgr/Processor] constructor: "+e+"\n");
		}
	}
	Processor.prototype={
		run: function() {
			//dump("[ConvertMgr/Processor] execConvert [run]\n");
			try {
				var result=false;
				if(this.dEntry.args2) {
					var file = Components.classes["@mozilla.org/file/directory_service;1"]
					                     .getService(Components.interfaces.nsIProperties)
					                     .get("TmpD", Components.interfaces.nsIFile);
					file.append("passlogfile.tmp");
					file.createUnique(Components.interfaces.nsIFile.NORMAL_FILE_TYPE, 0666);
					var r=this.convertPass(["-pass","1","-passlogfile",file.path].concat(this.dEntry.args),false);
					if(r) {
						result=this.convertPass(["-pass","2","-passlogfile",file.path].concat(this.dEntry.args2),true);
					}
					if(file.exists())
						file.remove(false);
				} else {
					result=this.convertPass(this.dEntry.args,true);
				}
				this.convMgr.scheduleNext(result);
			} catch(e) {
				dump("!!! [ConvertMgr/Processor] execConvert [run]: "+e+"\n");
			}
		},
		convertPass: function(args,lastPass) {
			var process = Components.classes["@mozilla.org/process/util;1"]
		                        .createInstance(Components.interfaces.nsIProcess);
			process.init(this.dEntry.file);

			process.run(true, args, args.length,{});
			
			var success;
			if(process.exitValue==0) {
				success=true;
				if(lastPass && this.dEntry.autoClear) {
					var keepOriginal=false;
					try {
						keepOriginal=this.pref.getBoolPref("convert.keep-original");
					} catch(e) {}
					if(!keepOriginal)
						this.dEntry.sourceFile.remove(false);
				}
			} else {
				success=false;
				if(this.dEntry.autoClear) {
					try {
						this.dEntry.targetFile.remove(false);
					} catch(e) {
						dump("!!! [ConvertMgr/Processor] execConvert [run] failed: "+e+"\n");
					}
					var keepOriginalOnFailure=true;
					try {
						keepOriginalOnFailure=this.pref.getBoolPref("convert.keep-original-on-failure");
					} catch(e) {}
					if(keepOriginalOnFailure) {
						var filename=this.dEntry.sourceFile.leafName;
						var recoveryLeafName=null;
						if(/^.+\..*$/.test(filename)) {
							var m=/^(.+)\.(.*)$/.exec(filename);
							recoveryLeafName=m[1]+".failed-conv."+m[2];
						} else {
							recoveryLeafName=filename+".failed-conv";
						}
						var recoveryFile=this.dEntry.targetFile.parent;
						recoveryFile.append(recoveryLeafName);
						recoveryFile.createUnique(Components.interfaces.nsIFile.NORMAL_FILE_TYPE, 0644);
						this.dEntry.sourceFile.moveTo(this.dEntry.targetFile.parent,recoveryFile.leafName);
					}
				}
			}
			return success;
		},
		QueryInterface: function(iid) {
			if (iid.equals(Components.interfaces.nsIRunnable) ||
				iid.equals(Components.interfaces.nsISupports)) {
				return this;
			}
			throw Components.results.NS_ERROR_NO_INTERFACE;
		}
		
	}
	try {
		var thread = Components.classes["@mozilla.org/thread;1"].createInstance(Components.interfaces.nsIThread);
		thread.init(
		        new Processor(this,dEntry),
		        0,
		        Components.interfaces.nsIThread.PRIORITY_LOW,
		        Components.interfaces.nsIThread.SCOPE_LOCAL,
		        Components.interfaces.nsIThread.STATE_UNJOINABLE
		        );
	} catch(e) {
		try {
			//dump("!!! [ConvertMgr/Processor] execConvert [creating thread]: "+e+"\n");
			var threadMgr = Components.classes["@mozilla.org/thread-manager;1"].getService();
			var thread=threadMgr.newThread(0);
			thread.dispatch(new Processor(this,dEntry),thread.DISPATCH_NORMAL);
			//dump("[ConvertMgr/Processor] execConvert [dispatched]\n");
		} catch(e) {
			dump("!!! [ConvertMgr/Processor] execConvert [dispatching]: "+e+"\n");
		}
	}
}

ConvertMgr.prototype.checkConvert=function() {
	try {
		if(this.delayQueue.length>0 && this.currentEntry==null) {
			var dEntry=this.delayQueue.shift();
			this.execConvert(dEntry);
		}
	} catch(e) {
		dump("!!! [ConvertMgr] checkConvert(): "+e+"\n");
	}
}

ConvertMgr.prototype.addConvert=function(sourceFile,targetFile,format,autoClear,listener,entry,ctx) {

	//dump("[ConvertMgr] addConvert("+sourceFile.path+","+targetFile.path+","+format+")\n");

	if(!this.isEnabled())
		return;
	this.convert(sourceFile,targetFile,format,autoClear,listener,entry,ctx);
}

ConvertMgr.prototype.getFormat=function(filename,mediaUrl,pageUrl) {

	//dump("[ConvertMgr] getFormat("+filename+","+mediaUrl+","+pageUrl+")\n");

	if(!this.isEnabled()) {
		//dump("[ConvertMgr] getFormat(): convert not enabled\n");
		return null;
	}
	var extension=null;
	try {
		extension=/.*\.(.*?)$/.exec(filename)[1];
	} catch(e) {
		dump("!!! [ConvertMgr] getFormat(): no extension from "+filename+"\n");
		return null;
	}
	try {
	var rules=Util.getChildResourcesS(this.datasource,"urn:root",{});
	for(var i=0;i<rules.length;i++) {
		var infile=Util.getPropertyValueRS(this.datasource,rules[i],DHNS+"infile");
		if(infile=="" || infile.toLowerCase()==extension.toLowerCase()) {
			var insite=Util.getPropertyValueRS(this.datasource,rules[i],DHNS+"insite");
			var re=new RegExp("https?://(?:[^/]*\\.)?"+insite+"/.*","i");
			if(insite=="" || re.test(mediaUrl) || (pageUrl!=null && re.test(pageUrl))) {
				var action=Util.getPropertyValueRS(this.datasource,rules[i],DHNS+"action");
				if(action=="0") {
					//dump("[ConvertMgr] getFormat(): explicit no conversion\n");
					return null;
				} else {
					var format=Util.getPropertyValueRS(this.datasource,rules[i],DHNS+"outformat");
					//dump("[ConvertMgr] getFormat(): format="+format+"\n");
					return format;
				}
			}
		}
	}
	return null;
	} catch(e) {
		dump("!!! [ConvertMgr] getFormat(): "+e+"\n");
		return null;
	}
}

ConvertMgr.prototype.getDataSource=function() {
	//dump("[ConvertMgr] getDataSource()\n");
	return this.datasource;
}

ConvertMgr.prototype.setDataSource=function(datasource) {
	//dump("[ConvertMgr] setDataSource()\n");
	try {
		var serializer="@mozilla.org/rdf/xml-serializer;1";
		var s=Components.classes[serializer].createInstance(Components.interfaces.nsIRDFXMLSerializer);
		s.init(datasource);
		var stream = Components.classes['@mozilla.org/network/file-output-stream;1']
		    .createInstance(Components.interfaces.nsIFileOutputStream);
		stream.init(this.convRulesFile, 42, 0644, 0); 
	
		s.QueryInterface(Components.interfaces.nsIRDFXMLSource).Serialize(stream);
		stream.close();
	} catch(e) {
		dump("!!! [ConvertMgr] setDataSource: "+e+"\n");
	}
	this.datasource=datasource;
}

ConvertMgr.prototype.getDataSourceCopy=function() {
	//dump("[ConvertMgr] getDataSourceCopy()\n");
	var datasource=Util.getDatasourceFromRDFFile(this.convRulesFile);
	return datasource;
}

ConvertMgr.prototype.makeDefaultRule=function(datasource) {
	//dump("[ConvertMgr] makeDefaultRule()\n");
	var defRule=Util.createAnonymousNodeS(datasource,"urn:root");
	Util.setPropertyValueRS(datasource,defRule,DHNS+"label",Util.getText("conversion.default-rule-label"));
	Util.setPropertyValueRS(datasource,defRule,DHNS+"action","1");
	Util.setPropertyValueRS(datasource,defRule,DHNS+"infile","flv");
	Util.setPropertyValueRS(datasource,defRule,DHNS+"insite","");
	Util.setPropertyValueRS(datasource,defRule,DHNS+"outformat","wmv/-acodec wmav1 -b 1000kbps -f asf -vcodec wmv1");
	Util.setPropertyValueRS(datasource,defRule,DHNS+"outdir","");
	Util.setPropertyValueRS(datasource,defRule,DHNS+"label",this.makeLabel(datasource,defRule.Value));
	return defRule.Value;
}

ConvertMgr.prototype.isEnabled=function() {
	//dump("[ConvertMgr] isEnabled()\n");
	var enabled=false;
	try {
		enabled=this.pref.getBoolPref("conversion-enabled");
	} catch(e) {}
	return enabled;
}

ConvertMgr.prototype.getInstallDir=function() {
	//dump("[ConvertMgr] getInstallDir()\n");
	try {
		var wrk = Components.classes["@mozilla.org/windows-registry-key;1"]
		                    .createInstance(Components.interfaces.nsIWindowsRegKey);
		if(wrk==null) {
			dump("!!![ConvertMgr] getInstallDir(): no registry service\n");
			return null;
		}
		
		var method=this.getConvMethod();
		var regPath;
		if(method==CONV_METHOD_WIN_DH)
			regPath="SOFTWARE\\DownloadHelper\\ConvertHelper";
		else {
			dump("!!![ConvertMgr] getInstallDir(): no path available\n");
			return null;
		}
		var r=wrk.open(wrk.ROOT_KEY_LOCAL_MACHINE,
		         regPath,
		         wrk.ACCESS_READ);
		//dump("[ConvertMgr] getExePath(): open returns "+r+"\n");
		var folderPath = wrk.readStringValue("InstallFolder");
		wrk.close();
		//dump("*** "+folderPath+"\n");
		if(folderPath==null)
			return null;
		var file = Components.classes["@mozilla.org/file/local;1"]
		                     .createInstance(Components.interfaces.nsILocalFile);
		file.initWithPath(folderPath);
		if(!file.exists()) {
			dump("!!![ConvertMgr] getInstallDir(): "+folderPath+" does not exist\n");
			return;
		}
		if(!file.isDirectory()) {
			dump("!!![ConvertMgr] getInstallDir(): "+folderPath+" is not a directory\n");
			return;
		}
		if(!file.isReadable()) {
			dump("!!![ConvertMgr] getInstallDir(): "+folderPath+" is not readable\n");
			return;
		}
		return file;
		
	} catch(e) {
		dump("!!![ConvertMgr] getInstallDir():"+e+"\n");
		return null;
	}
}

ConvertMgr.prototype.clearDataSource=function(datasource) {
	var i = datasource.GetAllResources();
	datasource.beginUpdateBatch();
	while(i.hasMoreElements()) {
		var source = i.getNext();
		var j = datasource.ArcLabelsOut(source);
		while(j.hasMoreElements()) {
			var predicate = j.getNext();
			var k = datasource.GetTargets(source,predicate,true);
			while(k.hasMoreElements()) {
				var target = k.getNext();
				datasource.Unassert(source,predicate,target);
			}
		}
	}
	datasource.endUpdateBatch();	
}

ConvertMgr.prototype.makeEmptyDataSource=function(datasource) {
	this.clearDataSource(datasource);
}

ConvertMgr.prototype.makeDefaultDataSource=function(datasource) {
	this.clearDataSource(datasource);
    this.makeDefaultRule(datasource);
}

ConvertMgr.prototype.makeLabel=function(datasource,ref) {
	try {
	var action=Util.getPropertyValueSS(datasource,ref,
		"http://downloadhelper.net/1.0#action");
	var insite=Util.getPropertyValueSS(datasource,ref,
		"http://downloadhelper.net/1.0#insite");
	var infile=Util.getPropertyValueSS(datasource,ref,
		"http://downloadhelper.net/1.0#infile");
	var outformat=Util.getPropertyValueSS(datasource,ref,
		"http://downloadhelper.net/1.0#outformat");
	var outdir=Util.getPropertyValueSS(datasource,ref,
		"http://downloadhelper.net/1.0#outdir");
	
	if(insite=="")
		insite=Util.getText("label.conv-rule.all-sites");
	if(infile=="")
		infile=Util.getText("label.conv-rule.all-files");
	if(outdir=="")
		outdir=Util.getText("label.conv-rule.default-directory");
	
	var label;
	if(action=="0") {
		label=Util.getFText("label.conv-rule.label.do-not-convert",[
			infile, insite
		],2);
	} else {
		var format=/^(.*?)\/.*$/.exec(outformat)[1];
		label=Util.getFText("label.conv-rule.label.convert",[
			infile, insite,
			format.toUpperCase(),
			outdir
		],4);
	}
			
	return label;
		
	} catch(e) {
		return null;
	}
}

ConvertMgr.prototype.updateUnregistered = function() {
	var cf=true;
	
	var method=this.getConvMethod();
	if(this.isEnabled() && method==CONV_METHOD_WIN_DH) {
		try {
			var reg=Components.classes["@mozilla.org/windows-registry-key;1"]
				.createInstance(Components.interfaces.nsIWindowsRegKey);
			try {
				reg.open(reg.ROOT_KEY_CURRENT_USER,
					         "SOFTWARE\\DownloadHelper\\ConvertHelper",
					         reg.ACCESS_READ);
			    var name=reg.readStringValue("CustomerName");
			    var email=reg.readStringValue("CustomerEmail");
			    var key=reg.readStringValue("LicenseKey");
			    var licenseCheck=this.md5("converthelper"+key+name+email);
			    var licenseCheck0=reg.readStringValue("LicenseCheck");
			    reg.close();
			    if(licenseCheck==licenseCheck0) { 
			    	var profile=Util.getProfileDir().leafName;
			    	var profileCheck=this.md5("converthelper"+profile);
					var profileCheck0="";
					try {
						profileCheck0=this.pref.getCharPref("converthelper-key");
					} catch(e) {}
					if(profileCheck!=profileCheck0) {
						this.checkLicense(key);
					} else {
						cf=false;
					}
				}    
			} catch(e) { 
				//dump("!!![ConvertMgr] updateUnregistered: "+e+"\n"); 
			}
			try {
				reg.close();
			} catch(e) {}
		} catch(e) { 
			cf=false; 
		}
	} else if(method==CONV_METHOD_UNIX) {
		cf=false;
	}

	this.pref.setBoolPref("convert-free",cf);
	
	return cf;
}

ConvertMgr.prototype.register = function(code) {

	//dump("register: "+code+"\n");


	var method=this.getConvMethod();
	if(method==CONV_METHOD_WIN_DH) {
		try {
			this.checkLicense(code);
		} catch(e) {
			//dump("register exception: "+e+"\n");
		}
	}
}

ConvertMgr.prototype.getConvMethod = function() {
	var method=CONV_METHOD_NONE;
	try {
		var reg = Components.classes["@mozilla.org/windows-registry-key;1"]
		                    .createInstance(Components.interfaces.nsIWindowsRegKey);
		method=CONV_METHOD_WIN_DH;
	} catch(e) {
		return CONV_METHOD_UNIX;
	}
	return method;
}

ConvertMgr.prototype.checkLicense = function(key) {
	
	function XMLStreamListener(service) {
		this.service=service;
	}
	
	XMLStreamListener.prototype={
		QueryInterface: function(iid) {
		    if (!iid.equals(Components.interfaces.nsISupports) && 
		    	!iid.equals(Components.interfaces.nsIStreamListener)) {
		            throw Components.results.NS_ERROR_NO_INTERFACE;
		        }
		    return this;
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
				try {
					var parser=Components.classes["@mozilla.org/xmlextras/domparser;1"].
						createInstance(Components.interfaces.nsIDOMParser);
					var doc=parser.parseFromString(this.data,"text/xml");
					var xml=doc.documentElement;
					var status=Util.xpGetString(xml,"/check-license-response/status/text()");
				    var name=Util.xpGetString(xml,"/check-license-response/name/text()");
				    var email=Util.xpGetString(xml,"/check-license-response/email/text()");
					if(status=="accepted") {
						var reg = Components.classes["@mozilla.org/windows-registry-key;1"]
	            	        .createInstance(Components.interfaces.nsIWindowsRegKey);
						reg.open(reg.ROOT_KEY_CURRENT_USER,
					         "SOFTWARE",
					         reg.ACCESS_ALL);
				    	reg=reg.createChild("DownloadHelper\\ConvertHelper",
					         reg.ACCESS_ALL);
					    var existingKey=null;
					    try {
							existingKey=reg.readStringValue("LicenseKey");
					    } catch(e) {}
					    reg.writeStringValue("CustomerName",name);
					    reg.writeStringValue("CustomerEmail",email);
					    reg.writeStringValue("LicenseKey",key);
					    var licenseCheck=this.service.md5("converthelper"+key+name+email);
					    reg.writeStringValue("LicenseCheck",licenseCheck);
					    reg.close();
					    var profile=Util.getProfileDir().leafName;
					    var profileCheck=this.service.md5("converthelper"+profile);
					    this.service.pref.setCharPref("converthelper-key",profileCheck);
					    this.service.updateUnregistered();
					    if(existingKey==null) {
							this.service.promptService.alert(null,Util.getText("title.converter-registration"),
								Util.getText("message.converter-registration-succeeded"));
					    }
					} else if(status=="need-validation") {
						this.service.promptService.alert(null,Util.getText("title.converthelper.revalidate"),
							Util.getFText("message.converthelper.revalidate",
								[name,email],2));
					} else {
						this.service.promptService.alert(null,Util.getText("title.converthelper.invalid-license"),
							Util.getText("message.converthelper.invalid-license"));
					}
				} catch(e) {
					dump("!!! [ConvertMgr] checkLicense: "+e+"\n"); 
				}
			} else {
				dump("!!! [ConvertMgr] checkLicense: response "+responseStatus+"\n"); 
			}
		}
	}
	var body="<check-license>\n"+
		"  <license-key>"+key+"</license-key>\n"+
		"  <product>converthelper</product>\n"+
		"</check-license>";
	var ios=Components.classes["@mozilla.org/network/io-service;1"]
	                           .getService(Components.interfaces.nsIIOService);
	var uri = ios.newURI("http://www.downloadhelper.net/license-check.php", null, null);
	var channel = ios.newChannelFromURI(uri);
	var httpChannel = channel.QueryInterface(Components.interfaces.nsIHttpChannel);
	httpChannel.requestMethod = "POST";
	var datais = Components.classes["@mozilla.org/io/string-input-stream;1"]
	                                .createInstance(Components.interfaces.nsIStringInputStream);
	datais.setData(body, body.length);
	var uploadChannel = channel.QueryInterface(Components.interfaces.nsIUploadChannel);
	uploadChannel.setUploadStream(datais, "application/x-binary", -1);
	var listener = new XMLStreamListener(this);
	channel.notificationCallbacks = listener;
	channel.asyncOpen(listener, null);
}

ConvertMgr.prototype.md5 = function(str) {
	var converter =
	  Components.classes["@mozilla.org/intl/scriptableunicodeconverter"].
	    createInstance(Components.interfaces.nsIScriptableUnicodeConverter);
	
	converter.charset = "UTF-8";
	var result = {};
	var data = converter.convertToByteArray(str, result);
	var ch = Components.classes["@mozilla.org/security/hash;1"]
	                   .createInstance(Components.interfaces.nsICryptoHash);
	ch.init(ch.MD5);
	ch.update(data, data.length);
	var hash = ch.finish(false);
	
	function toHexString(charCode)
	{
	  return ("0" + charCode.toString(16)).slice(-2);
	}
	
	var t=[];
	for(var i in hash) {
		t.push(toHexString(hash.charCodeAt(i)));
	}
	var s=t.join("");
	
	return s;
}

ConvertMgr.prototype.getInfo = function() {

	this.updateUnregistered();

	var props = Components.classes["@mozilla.org/properties;1"].createInstance(Components.interfaces.nsIProperties);

	var enabled=false;
	try {
		enabled=this.pref.getBoolPref("conversion-enabled");
	} catch(e) {}
	var sEnabled = Components.classes["@mozilla.org/supports-PRBool;1"].createInstance(Components.interfaces.nsISupportsPRBool);
	sEnabled.data=enabled;
	props.set("enabled",sEnabled);

	var method=this.getConvMethod();
	var sMethod = Components.classes["@mozilla.org/supports-PRInt32;1"].createInstance(Components.interfaces.nsISupportsPRInt32);
	sMethod.data=method;
	props.set("method",sMethod);

	var unregistered=true;
	try {
		unregistered=this.pref.getBoolPref("convert-free");
	} catch(e) {}
	var sUnregistered = Components.classes["@mozilla.org/supports-PRBool;1"].createInstance(Components.interfaces.nsISupportsPRBool);
	sUnregistered.data=unregistered;
	props.set("unregistered",sUnregistered);

	if(method==CONV_METHOD_WIN_DH) {

		var exeFound=false;			
		var reg=Components.classes["@mozilla.org/windows-registry-key;1"]
			.createInstance(Components.interfaces.nsIWindowsRegKey);
		try {
			reg.open(reg.ROOT_KEY_LOCAL_MACHINE,
				         "SOFTWARE\\DownloadHelper\\ConvertHelper",
				         reg.ACCESS_READ);

			try {
				var folderPath = reg.readStringValue("InstallFolder");
				var file = Components.classes["@mozilla.org/file/local;1"]
				                     .createInstance(Components.interfaces.nsILocalFile);
				file.initWithPath(folderPath);
				file.append("cvhelper.exe");
				if(file.exists()) {
					exeFound=true;
				}
			} catch(e) {}
			
			try {
				var version = reg.readStringValue("Version");
				var sVersion = Components.classes["@mozilla.org/supports-string;1"].createInstance(Components.interfaces.nsISupportsString);
				sVersion.data=version;
				props.set("converterversion",sVersion);
			} catch(e) {}

			try {
				var sVersion = Components.classes["@mozilla.org/supports-string;1"].createInstance(Components.interfaces.nsISupportsString);
				sVersion.data="2.0";
				props.set("converterminversion",sVersion);
			} catch(e) {}

			reg.close();

			reg.open(reg.ROOT_KEY_CURRENT_USER,
				         "SOFTWARE\\DownloadHelper\\ConvertHelper",
				         reg.ACCESS_READ);
			
			if(unregistered==false) {

			    var name=reg.readStringValue("CustomerName");
			    var email=reg.readStringValue("CustomerEmail");
			    var key=reg.readStringValue("LicenseKey");
	
				var sName = Components.classes["@mozilla.org/supports-string;1"].createInstance(Components.interfaces.nsISupportsString);
				sName.data=name;
				props.set("customername",sName);
	
				var sEmail = Components.classes["@mozilla.org/supports-string;1"].createInstance(Components.interfaces.nsISupportsString);
				sEmail.data=email;
				props.set("customeremail",sEmail);
	
				var sLicense = Components.classes["@mozilla.org/supports-string;1"].createInstance(Components.interfaces.nsISupportsString);
				sLicense.data=key;
				props.set("license",sLicense);
			}
		} catch(e) {}

		var sExeFound = Components.classes["@mozilla.org/supports-PRBool;1"].createInstance(Components.interfaces.nsISupportsPRBool);
		sExeFound.data=exeFound;
		props.set("exefound",sExeFound);

		try {
			reg.close();
		} catch(e) {}
	}

	var windows=false;
	try {
		var reg=Components.classes["@mozilla.org/windows-registry-key;1"]
			.createInstance(Components.interfaces.nsIWindowsRegKey);
		windows=true;
	} catch(e) {}
	var sWindows = Components.classes["@mozilla.org/supports-PRBool;1"].createInstance(Components.interfaces.nsISupportsPRBool);
	sWindows.data=windows;
	props.set("windows",sWindows);

	if(!windows) {
		var exeFound=false;
		var encoder="ffmpeg";
		try {
			encoder=this.pref.getCharPref("preferred-converter");
		} catch(e) {}
		try {
			var encoderFile = Components.classes["@mozilla.org/file/local;1"]
			                                     .createInstance(Components.interfaces.nsILocalFile);
			encoderFile.initWithPath(this.pref.getCharPref("converter-path-"+encoder));
			if(encoderFile.exists()) {
				exeFound=true;
			}
		} catch(e) {}
		var sExeFound = Components.classes["@mozilla.org/supports-PRBool;1"].createInstance(Components.interfaces.nsISupportsPRBool);
		sExeFound.data=exeFound;
		props.set("exefound",sExeFound);
	}
	
	return props;
}

ConvertMgr.prototype.getQueueDataSource = function() {
	return this.queueDatasource;
}

ConvertMgr.prototype.checkConverterVersion = function() {
	var props=this.getInfo();
	var version="1.0";
	try {
		version=props.get("converterversion",Components.interfaces.nsISupportsString).data;
	} catch(e) {}
	var minVersion="1.0";
	try {
		minVersion=props.get("converterminversion",Components.interfaces.nsISupportsString).data;
	} catch(e) {}
	if(parseFloat(version)<parseFloat(minVersion)) {
		var r=this.promptService.confirm(null,Util.getText("error.convert-helper.version.title"),
			Util.getFText("error.convert-helper.version",[version,minVersion],2));
		if(r) {
			var wwatch = Components.classes["@mozilla.org/embedcomp/window-watcher;1"].getService().
				QueryInterface(Components.interfaces.nsIWindowWatcher);
			wwatch.openWindow(null, "http://www.downloadhelper.net/install-converter.php",null,null,null);
		}
		return false;
	}
	return true;
}

ConvertMgr.prototype.scheduleNext = function(result) {
	if(result) {
		Util.setPropertyValueRS(this.queueDatasource,this.currentEntry.qRes,DHNS+"Status","4");
		Util.setPropertyValueRS(this.queueDatasource,this.currentEntry.qRes,DHNS+"EndDate",""+new Date());
		var cvcount=0;
		try {
			cvcount=this.pref.getIntPref("convert-count");
		} catch(e) {
		}
		cvcount++;
		this.pref.setIntPref("convert-count",cvcount);
		try {
			this.convConfShare(this.currentEntry.convConf);
		} catch(e) {
			dump("!!! [ConvertMgr] convConfShare: "+e+"\n");
		}
	} else {
		Util.setPropertyValueRS(this.queueDatasource,this.currentEntry.qRes,DHNS+"Status","5");
		Util.setPropertyValueRS(this.queueDatasource,this.currentEntry.qRes,DHNS+"EndDate",""+new Date());
		Util.setPropertyValueRS(this.queueDatasource,this.currentEntry.qRes,DHNS+"ErrorMessage","Converter error");
	}
	this.showNotification(this.currentEntry,result);
	if(this.currentEntry.listener) {
		try {
			this.currentEntry.listener.conversionFinished(result,this.currentEntry.entry,this.currentEntry.ctx);
		} catch(e) {
			dump("!!! [ConvertMgr] scheduleNext()/listener: "+e+"\n");
		}
	}
	this.currentEntry=null;
	this.checkConvert();
}

ConvertMgr.prototype.showNotification = function(entry,result) {
	try {
		var showNotification=true;
		try {
			showNotification=this.pref.getBoolPref("convert-helper.notification");
		} catch(e) {
		}
		if(showNotification) {
			var alertsService = Components.classes["@mozilla.org/alerts-service;1"]
			                    .getService(Components.interfaces.nsIAlertsService);
			var title=Util.getText("title.notif.conversion");
			var message;
			if(result)
				message=Util.getFText("message.notif.conversion.success",[entry.targetFile.leafName],1);
			else
				message=Util.getFText("message.notif.conversion.failure",[entry.targetFile.leafName],1);
			alertsService.showAlertNotification("chrome://dwhelper/skin/converter-16x16.png",
				title,message);
		}
	} catch(e) {
		//dump("!!![ConvertMgr] showNotification: "+e+"\n");
	}
}

ConvertMgr.prototype.convConfShare = function(convConf) {
	var confs=Util.getChildResourcesS(this.localstore,DHNS+"conv-confs",{});
	var convCount=0;
	if(!isNaN(parseInt(Util.getPropertyValueSS(this.localstore,DHNS+"conv-confs",DHNS+"conv-count"))))
		convCount=parseInt(Util.getPropertyValueSS(this.localstore,DHNS+"conv-confs",DHNS+"conv-count"));
	convCount++;
	Util.setPropertyValueSS(this.localstore,DHNS+"conv-confs",DHNS+"conv-count",""+convCount);
	var shareStep=0;
	if(!isNaN(parseInt(Util.getPropertyValueSS(this.localstore,DHNS+"conv-confs",DHNS+"share-step"))))
		shareStep=parseInt(Util.getPropertyValueSS(this.localstore,DHNS+"conv-confs",DHNS+"share-step"));
	var doPublish=false;
	if(convCount>=10*Math.pow(2,shareStep)) {
		doPublish=true;
		Util.setPropertyValueSS(this.localstore,DHNS+"conv-confs",DHNS+"share-step",""+(shareStep+1));
	}
	var doShare=true;
	try {
		doShare=this.pref.getBoolPref("convert-helper.share-config");
	} catch(e) {}
	if(doShare==false)
		doPublish=false;
	var xml="<?xml version='1.0'?>\n<conversion-configs>\n";
	for(var i=0;i<confs.length;i++) {
		/* dump("Checking conf "+Util.getPropertyValueRS(this.localstore,confs[i],DHNS+"label")+" = "+
				Util.getPropertyValueRS(this.localstore,confs[i],DHNS+"value")+"\n"); */
		var cnt=0;
		if(!isNaN(parseInt(Util.getPropertyValueRS(this.localstore,confs[i],DHNS+"conv-count"))))
			cnt=parseInt(Util.getPropertyValueRS(this.localstore,confs[i],DHNS+"conv-count"));
		var conf=Util.getPropertyValueRS(this.localstore,confs[i],DHNS+"value");
		if(conf==convConf) {
			cnt++;
			Util.setPropertyValueRS(this.localstore,confs[i],DHNS+"conv-count",""+cnt);
		}
		if(cnt>=10) {
			xml+="  <conversion-config>\n";
			xml+="    <value>"+Util.xmlEscape(Util.getPropertyValueRS(this.localstore,confs[i],DHNS+"value"))+"</value>\n";
			xml+="    <label>"+Util.xmlEscape(Util.getPropertyValueRS(this.localstore,confs[i],DHNS+"label"))+"</label>\n";
			xml+="    <count>"+cnt+"</count>\n";
			xml+="  </conversion-config>\n";
			Util.setPropertyValueRS(this.localstore,confs[i],DHNS+"conv-count","0");
		}
	}
	xml+="</conversion-configs>\n";
	if(doPublish) {
        var xmlhttp = Components.classes["@mozilla.org/xmlextras/xmlhttprequest;1"].
        	createInstance(Components.interfaces.nsIXMLHttpRequest);
	    xmlhttp.open ("POST", "http://www.downloadhelper.net/share-conv-confs.php")
        xmlhttp.send(xml);
	}
}

ConvertMgr.prototype.setDefaultConfigs = function() {
	var formats=[
	             {
	            	 value: 'mpg/-acodec mp2 -b 800kbps -f mpeg -r 24 -vcodec mpeg1video',
	            	 title: 'MPEG (mpeg1+mp2)'
	             },
	             {
	            	 value: 'wmv/-acodec wmav1 -b 1000kbps -f asf -vcodec wmv1',
	            	 title: 'WMV (Windows Media Player)'
	             },
	             {
	            	 value: 'm4v/-acodec libfaac -b 274kbps -f mp4 -r 24 -s 320x240 -vcodec mpeg4',
	            	 title: 'iPod'
	             },
	             {
	            	 value: 'm4v/-acodec libfaac -b 548kbps -f mp4 -r 24 -s 480x320 -vcodec mpeg4',
	            	 title: 'iPhone'
	             },
	             {
	            	 value: '3gp/-ab 12kbps -ac 1 -acodec libfaac -ar 8000 -b 64kbps -f 3gp -r 24 -s 176x144 -vcodec h263',
	            	 title: 'Mobile 3GP (Qcif)'
	             },
	             {
	            	 value: 'wmv/-ab 128000 -ac 2 -acodec wmav2 -b 640000 -bufsize 2048000 -f asf -maxrate 1350000 -s 320x240 -vcodec wmv2',
	            	 title: 'Zune'
	             },
	             {
	            	 value: 'mov/-f mov -sameq',
	            	 title: 'Quicktime (MOV)'
	             },
	             {
	            	 value: 'avi/-acodec libmp3lame -f avi -vcodec mpeg4',
	            	 title: 'AVI (mpeg4+mp3)'
	             },
	             {
	            	 value: 'mp4/-ac 2 -acodec libmp3lame -ar 44100 -f avi -vcodec mpeg4',
	            	 title: 'MPEG-4 '
	             },
	             {
	            	 value: 'mpeg/-f mpeg2video -target pal-dvd',
	            	 title: 'MPEG-2 DVD (PAL)'
	             },
	             {
	            	 value: 'mpeg/-f mpeg2video -target ntsc-dvd',
	            	 title: 'MPEG-2 DVD (NTSC)'
	             },
	             {
	            	 value: 'mp3/-ab 128k -f mp3',
	            	 title: 'MP3'
	             },
	];
	this.localstore.beginUpdateBatch();
	if(Util.getPropertyResourceSS(this.localstore,DHNS+"conv-confs","http://www.w3.org/1999/02/22-rdf-syntax-ns#instanceOf")==null) {
		Util.createNodeRS(this.localstore,null,DHNS+"conv-confs");
	} 
	var convConfs=DHNS+"conv-confs";
	for(var i=0;i<formats.length;i++) {
		var format=formats[i];
		var convConf=Util.createNodeSS(this.localstore,convConfs,null);
		Util.setPropertyValueRS(this.localstore,convConf,DHNS+"value",format.value);
		Util.setPropertyValueRS(this.localstore,convConf,DHNS+"label",format.title);
	}
	this.localstore.endUpdateBatch();
}

ConvertMgr.prototype.clearConfigs = function(full) {
	this.localstore.beginUpdateBatch();
	var confs=Util.getChildResourcesS(this.localstore,DHNS+"conv-confs",{});
	for(var i=0;i<confs.length;i++) {
		Util.removeReference(this.localstore,confs[i]);
	}
	if(full)
		Util.removeReferenceS(this.localstore,DHNS+"conv-confs");
	this.localstore.endUpdateBatch();
}

ConvertMgr.prototype.getConvertedFileName = function(filename,format) {
	var extension=null;
	if(format!=null && /^.+\/.*/.test(format))
		extension=/^(.+?)\/.*$/.exec(format)[1];
	if(extension!=null) {
		if(/^.+\..+$/.test(filename)) {
			filename=/^(.+\.)[^\.]{1,5}$/.exec(filename)[1]+extension;
		} else {
			filename=filename+"."+extension;
		}
	}
	return filename;
}

ConvertMgr.prototype.checkConverter= function(interactive) {
	//dump("[ConvertMgr] checkConverter()\n");
    var wm = Components.classes["@mozilla.org/appshell/window-mediator;1"]
                                .getService(Components.interfaces.nsIWindowMediator);
	var window = wm.getMostRecentWindow("navigator:browser");
	var info=this.getInfo();
	while(info.get("enabled",Components.interfaces.nsISupportsPRBool).data==false || 
			info.get("exefound",Components.interfaces.nsISupportsPRBool).data==false) {
		if(!interactive)
			return false;
		var msgTag="";
		if(info.get("enabled",Components.interfaces.nsISupportsPRBool).data==false)
			msgTag="confirm.conversion-not-enabled.configure";
		else if(info.get("exefound",Components.interfaces.nsISupportsPRBool).data==false)
			msgTag="confirm.converter-not-found.configure";
		var rc=window.confirm(Util.getText(msgTag));
		if(!rc)
			return false;
	    var options="chrome,centerscreen,titlebar,toolbar,modal";
	    var data={ selectedPanel: "panel-conversion" }
	    window.openDialog("chrome://dwhelper/content/preferences-new.xul",'_blank',options, data );
	    info=this.getInfo();
	}
	//dump("[ConvertMgr] checkConverter() => true\n");
	return true;
}

ConvertMgr.prototype.QueryInterface = function(iid) {
    if(
    	iid.equals(Components.interfaces.dhIConvertMgr)==false &&
    	iid.equals(Components.interfaces.nsISupports)==false
	) {
            throw Components.results.NS_ERROR_NO_INTERFACE;
        }
    return this;
}

var vConvertMgrModule = {
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
        compMgr.registerFactoryLocation(NS_CONVERT_MGR_CID,
                                        "ConvertMgr",
                                        NS_CONVERT_MGR_PROG_ID, 
                                        fileSpec,
                                        location,
                                        type);
    },

	unregisterSelf: function(compMgr, fileSpec, location) {
    	compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    	compMgr.unregisterFactoryLocation(NS_DH_CONVERT_MGR_CID, fileSpec);
	},

    /*
     * The GetClassObject method is responsible for producing Factory and
     * SingletonFactory objects (the latter are specialized for services).
     */
    getClassObject: function (compMgr, cid, iid) {
        if (!cid.equals(NS_CONVERT_MGR_CID)) {
	    	throw Components.results.NS_ERROR_NO_INTERFACE;
		}

        if (!iid.equals(Components.interfaces.nsIFactory)) {
	    	throw Components.results.NS_ERROR_NOT_IMPLEMENTED;
		}

        return this.vConvertMgrFactory;
    },

    /* factory object */
    vConvertMgrFactory: {
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

			return new ConvertMgr().QueryInterface(iid);
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
    return vConvertMgrModule;
}

