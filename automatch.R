######################
#### parametros
######################
 
lang="en" # ("sp" for spanish or "en" for english)
d1correctedname<-"d1corrected.csv"
d2correctedname<-"d2corrected.csv"

######################
#### Messages
######################
ignoreWarning<-list()
ignoreWarning[["sp"]]<-"Si hay warning, no apurarse y continuar."
ignoreWarning[["en"]]<-"You can safely ignore warnings and go ahead."
columnsOnlyIn<-list()
columnsOnlyIn[["sp"]]<-"Columnas solo en:"
columnsOnlyIn[["en"]]<-"Columns only in:"
referenceColumns<-list()
referenceColumns[["sp"]]<-"Escoger las columnas que van a servir de referencia:"
referenceColumns[["en"]]<-"Choose reference columns:"
enterNumColRef<-list()
enterNumColRef[["sp"]]<-"Entrar num columna separados con coma:"
enterNumColRef[["en"]]<-"Enter the number of the columns, separated by comas:"
tryAgain<-list()
tryAgain[["sp"]]<-"Intentar otra vez?"
tryAgain[["en"]]<-"Try again?"
dontUnderstand<-list()
dontUnderstand[["sp"]]<-"No entiendo:"
dontUnderstand[["en"]]<-"I don't understand:"
byeMessage<-list()
byeMessage[["sp"]]<-"Hasta la proxima!"
byeMessage[["en"]]<-"Bye!"
followingDuplicates<-list()
followingDuplicates[["sp"]]<-"Siguientes duplicados en"
followingDuplicates[["en"]]<-"Following duplicates in"
dealWithDuplicates<-list()
dealWithDuplicates[["sp"]]<-"Por favor escoger mejor los campos de referencia o\neliminar estos duplicados"
dealWithDuplicates[["en"]]<-"Please better chosse the reference fields \n or eliminate these duplicates" 
diffNbLines<-list()
diffNbLines[["sp"]]<-"No hay el mismo numero de lineas:"
diffNbLines[["en"]]<-"Tables have different number of lines:"
tryFoundMissingLines<-list()
tryFoundMissingLines[["sp"]]<-"Intento identificar las lineas que faltan."
tryFoundMissingLines[["en"]]<-"Try to find the missing lines."
linesOnlyIn<-list()
linesOnlyIn[["sp"]]<- "Lineas solo en"
linesOnlyIn[["en"]]<- "Lines only in"
usingFields<-list()
usingFields[["sp"]]<-"Utilizando:"
usingFields[["en"]]<-"Using fields:"
thereIs<-list()
thereIs[["sp"]]<-"Hay:"
thereIs[["en"]]<-"There are:"
errorsIn<-list()
errorsIn[["sp"]]<-"errores en"
errorsIn[["en"]]<-"errors in"
linesMes<-list()
linesMes[["sp"]]<-"linias"
linesMes[["en"]]<-"lines"
errorMes<-list()
errorMes[["sp"]]<-"Error"
errorMes[["en"]]<-"Error"
deMes<-list()
deMes[["sp"]]<-"de"
deMes[["en"]]<-"of"
menuNoChange<-list()
menuNoChange[["sp"]]<-"No se corrigio nada. Usted quiere:
	s: stop? Parar la correccion (se va a salvar el estado corriente)?
	r: regresar. Corregir de nuevo este error.
	C: continuar.\n 
	"
menuNoChange[["en"]]<-"You didn't change anything. Do you want to:
	s: stop? Stop to correct (your changes will be saved).
	r: go back. Go back to correction windows of this error.
	C: continue.\n 
	"
errorMatchCommon<-list()
errorMatchCommon[["sp"]]<- "Hay problemas, hacer el match con parte commun de las tablas?"
errorMatchCommon[["en"]]<- "There are problems, continue the match with only common part of both tables?"
errorsAnalyseAgain<-list()
errorsAnalyseAgain[["sp"]]<-"errores. Analysar de nuevo?"
errorsAnalyseAgain[["en"]]<-"errors. Analyse again?"
chooseProfil<-list()
chooseProfil[["sp"]]<-"Escoge perfil"
chooseProfil[["en"]]<-"Choose profile"
profileNumber<-list()
profileNumber[["sp"]]<-"Numero (nada permite entrar nuevo): "
profileNumber[["en"]]<-"Profile number (nothing allows to enter it again): "
dataBaseMes<-list()
dataBaseMes[["sp"]]<-"Base de datos"
dataBaseMes[["en"]]<-"Database"
userMes<-list()
userMes[["sp"]]<-"Usuario"
userMes[["en"]]<-"User"
passwordMes<-list()
passwordMes[["sp"]]<-"Contrasenia?"
passwordMes[["en"]]<-"Password?"
connectionOkMes<-list()
connectionOkMes[["sp"]]<-"Conneccion ok"
connectionOkMes[["en"]]<-"Connection ok"
saveConnectionMes<-list()
saveConnectionMes[["sp"]]<-"Guardar la conneccion?"
saveConnectionMes[["en"]]<-"Save connection?"
connectionName<-list()
connectionName[["sp"]]<-"Nombre connection? "
connectionName[["en"]]<-"Nombre connection? "
tableNameMySQL<-list()
tableNameMySQL[["sp"]]<-"Nombre tabla en MySQL? "
tableNameMySQL[["en"]]<-"Table name in MySQL? "
choiceReplaceMySQL<-list()
choiceReplaceMySQL[["sp"]]<-"Quiere remplazar todo, actualizar o aniadir?"
choiceReplaceMySQL[["en"]]<-"Do you want to replace all, update or add?"
tableNotSent<-list()
tableNotSent[["sp"]]<-"Tabla no enviada"
tableNotSent[["en"]]<-"Table not sent"
tableSentSuccess<-list()
tableSentSuccess[["sp"]]<-"Tabla exportada con exito"
tableSentSuccess[["en"]]<-"Table successfully sent exported"
tableNumber<-list()
tableNumber[["sp"]]<-"Numero de la tabla?"
tableNumber[["en"]]<-"Table number?"
importCsvOrMysql<-list()
importCsvOrMysql[["sp"]]<-"Importar csv o de mysql?"
importCsvOrMysql[["en"]]<-"Import from csv or from MySQL?"
originTableMes<-list()
originTableMes[["sp"]]<-"Origen de la tabla"
originTableMes[["en"]]<-"Origin of the table"
tableDestination<-list()
tableDestination[["sp"]]<-"Destinaccion de la tabla"
tableDestination[["en"]]<-"Destination of the table"
inFirstTable<-list()
inFirstTable[["sp"]]<-"En la primera tabla."
inFirstTable[["en"]]<-"In first table."
inSecondTable<-list()
inSecondTable[["sp"]]<-"En la secunda tabla."
inSecondTable[["en"]]<-"In second table."
getMissingMenu<-list()
getMissingMenu[["sp"]]<-"Sacar los que faltan para Primera tabla, Secunda o los Dos?"
getMissingMenu[["en"]]<-"Get the missing for first table(p), second table(s) or both(d)?"
savingIn<-list()
savingIn[["sp"]]<-"Salva las tablas corregidas en:"
savingIn[["en"]]<-"Saving corrected tables in:"
andMes<-list()
andMes[["sp"]]<-"y"
andMes[["en"]]<-"and"
commonLinesSaved<-list()
commonLinesSaved[["sp"]]<-"Nota: solo las lineas en ambas tablas son salvadas"
commonLinesSaved[["en"]]<-"Nota: only lines existing in both tables are saved"
exportTableMySQL<-list()
exportTableMySQL[["sp"]]<-"Exportar la tabla a MySQL?"
exportTableMySQL[["sp"]]<-"Export table to MySQL?"

######################
#### functiones
######################
RMySQLok<-suppressWarnings(require("RMySQL",quietly=TRUE))
graphok<-suppressWarnings(require(gWidgetstcltk,quietly=TRUE));
# cat(ignoreWarning[[lang]])
# graphok<- FALSE # para forcar la activation /desactivaccion

set_to<-function(x,init=c("NULL"),final=0){
# set all in init to final
# if possible to change the column to numeric do it
# init can be multiple, final has to be unique

	if(class(x) != 'data.frame'){
		stop('x must be a data.frame');
	}
	if(length(final)>1){
		warning("final is of length >1, will only use first item\n");
	}

	for(colname in names(x)){
# add levels if necessary to factors
		if(is.factor(x[,colname])){
			x[,colname] <- factor(x[,colname], levels=unique(c(levels(x[,colname]), final)));
		}
# change values
		sel<-which(x[,colname]%in%init);
		if(length(sel>0)){
			x[sel,colname]<-final
		}
		sel<-which(is.na(x[,colname]));
		if(length(which(is.na(init)))>0 & length(sel)>0){
			x[sel,colname]<-final
		}

# simplify vectors
		if(is.factor(x[,colname])){
			x[,colname]<-as.factor(x[,colname]);
			nbNA<-suppressWarnings(length(which(is.na(as.numeric(as.character(x[,colname]))))));
			if(nbNA==0){
				x[,colname]<-as.numeric(as.character(x[,colname]));
			}
		}
	}

	return(x);
}


### check structure
# check columnas
checkColumns<-function(d1,d2){
	solo1<-setdiff(names(d1),names(d2));
	solo2<-setdiff(names(d2),names(d1));
	if(length(solo1)>0 || length(solo2)){ # check for different names in columns
		stop(columnsOnlyIn[[lang]],"\n d1:",solo1,"\n d2:",solo2);
	}else{
		# reorder columns if needed so that d2 has the same order than d1
		d2<-d2[,match(names(d1),names(d2))]
	}
	return(list(d1=d1,d2=d2));
}
## identifica referencias
getRefCol<-function(db){
	# return the columns number as chosen by user
	cat(referenceColumns[[lang]],"\n");
	print(names(db));
	refcoltxt<-readline(paste(enterNumColRef[[lang]],"[1,2] "));
	refcol<-suppressWarnings(as.numeric(strsplit(refcoltxt,",")[[1]]));
	if(refcoltxt==""){
		refcol<-c(1,2);
	}
	if(length(which(is.na(refcol)))>0){
		cat(dontUnderstand[[lang]],refcoltxt,"\n");
		continue<-readline(paste(tryAgain[[lang]],"[Y/n] "));
		if(continue=="n"){
			stop(byeMessage);
		}else{
			refcol<-getRefCol(db);
		}
	}
	return(refcol);
}

getref<-function(df,refcol){
	ref<-df[,refcol[1]];
	for(refnum in refcol[-1]){
		ref<-paste(ref,df[,refnum],sep="_");
	}
	return(ref);
}
checkDuplicates<-function(ref1,ref2){
	## identifica si hay duplicados en cada tabla
	matchok<-TRUE
	dup1<-which(duplicated(ref1));
	dup2<-which(duplicated(ref2));
	if(length(dup1)>0){
		cat("\n",followingDuplicates[[lang]],"1:\n",ref1[dup1]);
		cat("\n\n",dealWithDuplicates[[lang]],"\n");
		matchok<-FALSE
	}
	if(length(dup2)>0){
		cat("\n",followingDuplicates[[lang]],"2:\n",ref2[dup2]);
		cat("\n\n",dealWithDuplicates[[lang]],"\n");
		matchok<-FALSE
	}
	return(matchok);
}

checkLines<-function(d1,d2,refcol1,refcol2=refcol1){
	# verifica que las bases son equivalentes y sin duplicados d'accuerdo
	# con las columnas de referencia
	linesok<-TRUE

	# get the references for each line in each base
	ref1<-getref(d1,refcol1); 
	ref2<-getref(d2,refcol2);

	# check numero de linias
	difdim<-(dim(d1)[1]!=dim(d2)[1]);
	if(difdim){
		cat("\n",diffNbLines[[lang]],"\nd1:",dim(d1)[1],"\nd2:",dim(d2)[1]);
		cat("\n",tryFoundMissingLines[[lang]]);
		linesok<-FALSE
	}
	# verifica duplicados para referencias
	linesok<-min(checkDuplicates(ref1,ref2),linesok);

	# identifia las linias solo en una de las bases
	solo1<-setdiff(ref1,ref2);
	solo2<-setdiff(ref2,ref1);
	if(length(solo1)>0){
		cat("\n",linesOnlyIn[[lang]],"1:\n",solo1,"\n");
		rm1<-which(ref1 %in% solo1); # remueve los que solo estan en 1
		d1<-d1[-rm1,];
		linesok<-FALSE
	}
	if(length(solo2)>0){
		cat("\n",linesOnlyIn[[lang]],"2:\n",solo2,"\n");
		rm2<-which(ref2 %in% solo2); # remueve los que solo estan en 2
		d2<-d2[-rm2,];
		linesok<-FALSE
	}

	return(list(ok=linesok,d1=d1,d2=d2,refcol=refcol1));
}
checkSimStruct<-function(initd1d2){
	d1<-initd1d2$d1
	d2<-initd1d2$d2
### verificacion de la similaridad de las bases
	d1d2 <- checkColumns(d1,d2);
	refcol<-getRefCol(d1d2$d1);
	cat(usingFields[[lang]],refcol,"\n");

	similard1d2<-checkLines(d1d2$d1,d1d2$d2,refcol,refcol);

	return(similard1d2);
}

get.errorarray<-function(d1,d2,errorlinias,errorcolumnas){
	# presentacion de los errores en una tabla de pequenio tamanio
	errorarray<-rbind(d1[errorlinias,errorcolumnas],d2[errorlinias,errorcolumnas]);
	shift<-length(errorlinias);
	juntos<-as.vector(rbind(1:shift,(shift+1):(2*shift)));
	errorarray<-errorarray[juntos,];
	# a<-edit(errorarray);
	return(errorarray);
}
getFactorsHomogeneous<-function(d1,d2){
	# d1 y d2 tienen que 
	for(col in 1:length(names(d1))){ # para cada columna
		c1<-d1[,col];
		c2<-d2[,col];

		# si son factors fuerza los levels
		if(is.factor(c1)||is.factor(c2)){
			lev<-unique(c(levels(c1),levels(c2)));
			d1[,col]<-factor(c1,levels=lev);
			d2[,col]<-factor(c2,levels=lev);
		}
	}
	return(list(d1=d1,d2=d2));
}

#### saca una matrice de mismo tamanio que d1/d2 con 1 si hay differencias
getDifMat<-function(d1,d2){
	# d1 y d2 deben de ser de mismo tamanio y bien ordenadas
	dif<-mat.or.vec(dim(d1)[1],dim(d1)[2]);
	for(col in 1:length(names(d1))){ # para cada columna
		c1<-d1[,col];
		c2<-d2[,col];

		# si son factors fuerza los levels
		if(is.factor(c1)||is.factor(c2)){
			lev<-unique(c(levels(c1),levels(c2)));
			c1<-factor(c1,levels=lev);
			c2<-factor(c2,levels=lev);
		}

		# salva las diferencias
		dif[,col]<-(c1!=c2);
	}
	return(dif);
}
CorrectMatch<-function(d1,d2,refcol,dif){
	# ofrece corregir las differencias dentre d1 y d2 
	# utilizando las columnas de referencia en refcol
	# y la matrice dif indicando las differencias para cada cellula
	# factores en d1 y d2 deben de tener los mismos levels

	# identifia las linias y columnas prolematicas
	errorperlinia<-apply(dif,1,sum);

	errorlinias<-which(errorperlinia>0);
	errorcolumnas<-c(refcol,which(apply(dif,2,sum)>0));

	cat(thereIs[[lang]], sum(errorperlinia),errorsIn[[lang]],length(errorlinias),linesMes[[lang]],"\n");

	# offer correction
	correc<-list();
	count<-1;
	while(count<=length(errorlinias)){
		linia <- errorlinias[count] 
		cat(errorMes[[lang]],count,deMes[[lang]],length(errorlinias),"\n");
		# correct a copy
		difcol<-c(refcol,which(dif[linia,]==TRUE));
		original1<<-cbind(d1[linia,difcol]);
		original2<<-cbind(d2[linia,difcol]);
		original<-rbind(original1,original2);
		correcto<-edit(original);
		# ofrece de parar si no hay correcciones
		if( isTRUE(all.equal(correcto,original,check.attributes=FALSE))){
		    continue<-readline(menuNoChange[[lang]])
		    if(continue=="s"){
		      break()
		    }else if(continue=="r"){
			count<-count-1
		    }		
		}

		# estar seguro que la nueva version se podra
		# copiar si hay nuevos levels de factors
		isfacts <- which(sapply(correcto, is.factor));
		for(col in isfacts){
			lev<-unique(c(levels(d1[,difcol[col]]),levels(correcto[,col])));
			d1[,difcol[col]]<-factor(d1[,difcol[col]],levels=lev);
			lev<-unique(c(levels(d2[,difcol[col]]),levels(correcto[,col])));
			d2[,difcol[col]]<-factor(d2[,difcol[col]],levels=lev);
		}

		# salvar la nueva version
		d1[linia,difcol]<-correcto[1,];
		d2[linia,difcol]<-correcto[2,];
		count<-count+1
	}
	return(list(d1=d1,d2=d2));
}
matchSimilar<-function(similard1d2){
### si hay problemas se puede parar aqui
	matchok<-similard1d2$ok;
	refcol<-similard1d2$refcol;

	d1<-similard1d2$d1;
	d2<-similard1d2$d2;
	if(! matchok){
		continue<-readline(paste("\n",errorMatchCommon[[lang]],"[y/N]"));
		if(continue=="y"){
			matchok<-TRUE; # va a hacer el match
		}else{
			matchok<-FALSE; # no va a hacer el match
		}
	}

	while(matchok){
### ordena y limpia los registros para permetir la comparacion
# ordena
		ref1<-getref(d1,refcol);
		ref2<-getref(d2,refcol);
		d1<-d1[order(ref1),];
		d2<-d2[order(ref2),];

# limpia de los NA que no pueden ser iguales a ellos mismos
		d1<-set_to(d1,init=c(NA,"NA"),final=0);
		d2<-set_to(d2,init=c(NA,"NA"),final=0);

# fuerza los factores en las dos tablas de tener los mismos levels
		h<-getFactorsHomogeneous(d1,d2);
		d1<-h$d1;
		d2<-h$d2;

### genera la matrice de errores (1 es error);
		dif<-getDifMat(d1,d2);

# get.errorarray(d1,d2,errorlinias,errorcolumnas) # errores resumidas

### verificacion linea por linea con possibilidad de correccion
		newd1d2<-CorrectMatch(d1,d2,refcol,dif);

### qualidad de la nueva version
		d1<-set_to(newd1d2$d1,init=c(NA,"NA"),final=0);
		d2<-set_to(newd1d2$d2,init=c(NA,"NA"),final=0);
### always save current version, just in case

		dif<-getDifMat(d1,d2);
		cat(thereIs[[lang]],sum(dif),errorsAnalyseAgain[[lang]],"[y/N]\n");
		continue<-readline();
		if(continue!="y"){
		  matchok<-FALSE;
		}
	}
	return(list(d1=d1,d2=d2));
}

simplifyEncode <- function(df){
	for(col in 1:length(names(df))){
		names(df)[col]<-iconv(names(df)[col],"utf-8","US-ASCII",sub="");
	}
	return(df);
}

## possible export to MySQL
if(.Platform$OS.type=="unix"){
	mysqlcfgfile<-"~/.my.cnf"
}else{
	mysqlcfgfile<-"c:/my.cnf"
}
dbConnectSaved<-function(){
	sqldriver<-dbDriver("MySQL");

	# check if exist conf file, if not create it
	if(!file.exists(mysqlcfgfile)){
		file.create(mysqlcfgfile)
		oldgroup<-FALSE
	}

	# identify the groups in .my.cnf
	cnf<-readLines(mysqlcfgfile);
	linesgroup<-grep("\\[",cnf,value=TRUE);
	if(length(linesgroup)>0){
		cat(chooseProfil[[lang]],":\n",sep="");
		print(linesgroup);
		# numgroup<-1
		numgroup<-readline(profileNumber[[lang]]);
		oldgroup<-(numgroup %in% 1:length(linesgroup));	
	}else{
		oldgroup<-FALSE
	}

	if(oldgroup){
		knowngroup<-linesgroup[as.integer(numgroup)]
		knowngroup<-sub("\\]","",sub("\\[","",knowngroup));
		con<-try(dbConnect(sqldriver,group=knowngroup));
	}else{
		host<-readline("host?[localhost] ");
		if(host==""){
			host<-"localhost";
		}
		db<-readline(paste(dataBaseMes[[lang]],"?[] "));

			if(.Platform$OS.type=="unix"){
				user<-system("whoami",intern=TRUE);
			}else{
				user<-"";
			}

		cat(userMes[[lang]]," [",user,"]:",sep="");
		input<-readline("");
		if(input!=""){
			user<-input
		}	
		password<-readline(paste(passwordMes[[lang]],"[] "));
		con<-try(dbConnect(sqldriver,user=user,password=password,dbname=db,host=host));
	}
	if(class(con)!="try-error"){
		cat(connectionOkMes[[lang]],"\n");
			if(!oldgroup){
			savegroup<-readline(paste(saveConnectionMes[[lang]],"[y/N] "));
			if(savegroup=="y"){
				newgroup<-readline(connectionName[[lang]]);
				cat("[",newgroup,"]\n",file=mysqlcfgfile,append=TRUE,sep="");
				cat("user =",user,"\n",file=mysqlcfgfile,append=TRUE);
				cat("password =",password,"\n",file=mysqlcfgfile,append=TRUE);
				cat("database =",db,"\n",file=mysqlcfgfile,append=TRUE);
				cat("host =",host,"\n",file=mysqlcfgfile,append=TRUE);
				
			}
		}
	}else{
		dbGetQuery(con, paste("SET NAMES 'utf8'"))
	}

	return(con);
}
dbExportMySQL<-function(df){
	con<-dbConnectSaved();
	
	print(dbListTables(con));
	tabla<-readline(tableNameMySQL[[lang]]);
	
	if(dbExistsTable(con,tabla)){
		ready<-FALSE;
		while(! ready){
			mode<-readline(paste(choiceReplaceMySQL[[lang]],"[r/z/a/q] ",sep=""));
			if(mode=="r"){
				append<-FALSE;
				overwrite<-TRUE;
				ready<-TRUE;
			}else if(mode=="a"){
				append<-TRUE;
				overwrite<-FALSE;
				ready<-TRUE;
			}else if(mode=="z"){
# UPDATE `UPCH-AQP`.`DENUNCIAS` SET `RESUL_LABOR` = '1' WHERE `DENUNCIAS`.`numero registro` =4;

			}else if(mode=="q"){
				cat(tableNotSent[[lang]],"\n");
				return(FALSE);
			}
		}
	}else{
		append<-FALSE;
		overwrite<-FALSE;
	}
	# df<-simplifyEncode(df);
	succes<-dbWriteTable(con,tabla,df,append=append,overwrite=overwrite);
	if(succes){
		cat(tableSentSuccess[[lang]],"\n");
	}else{
		cat("\n!!! Error\n\n");
	}
	dbDisconnect(con);
	return(succes);
}
dbImportMySQL<-function(){
	con<-dbConnectSaved();
	if(class(con)!="try-error"){
		listTables<-""
		numtabla<-0
		while(numtabla<1 || numtabla>length(listTables)){
			listTables<-dbListTables(con);
			print(listTables);
			numtabla<-as.integer(readline(tableNumber[[lang]]));
		}
		df<-dbReadTable(con,listTables[numtabla]);
	}else{
		df<-NULL
	}
	dbDisconnect(con);

	return(df);
}

importDfs<-function(){
	if(RMySQLok){
		origine<-readline(paste(importCsvOrMysql[[lang]],"[c/M] "));
	}else{
		origine<-"c"
	}
	if(origine=="c"){
		if(.Platform$OS.type=="unix" && graphok){
			d1name<-gfile();
			d2name<-gfile();
		}else{
			d1name<-file.choose()
			d2name<-file.choose()
		}
		d1<-read.csv(d1name);
		d2<-read.csv(d2name);
	}else{
		d1<-dbImportMySQL();
		d2<-dbImportMySQL();
	}
	return(list(d1=d1,d2=d2));
}
cpMySQL<-function(struct=TRUE){
	cat(originTableMes[[lang]],"?\n")
	df<-dbImportMySQL()
	if(struct){
		df<-df[1,]
	}
	cat("\n",tableDestination[[lang]],"?\n",sep="")
	dbExportMySQL(df)
	# usa mejor dbSendQuery
	# CREATE TABLE `local`.`Table2` (
	# 	`row_names` text,
	# 	`ID` bigint( 20 ) DEFAULT NULL ,
	# 	`qwerqw` text,
	# 	`qwerqwe` text
	# 	) ENGINE = MYISAM DEFAULT CHARSET = latin1;

	# INSERT INTO `local`.`Table2`
	# SELECT *
	# FROM `local`.`Table1` ;

	return(df)
}
vincula<-function(){
	initd1d2<-importDfs(); # recupera las tablas

	d1<-initd1d2$d1
	d2<-initd1d2$d2
	cat(inFirstTable[[lang]])
	refcol1<-getRefCol(d1); # cual columnas usar para el merge
	cat(inSecondTable[[lang]])
	refcol2<-getRefCol(d2); # cual columnas usar para el merge

	ref1<-getref(d1,refcol1); 
	ref2<-getref(d2,refcol2);

	cat(getMissingMenu[[lang]],"[p/s/D]")
	idMissing<-readline()
	if(idMissing=="p"){
		d2<-d2[ref2 %in% ref1,]
	}else if(idMissing=="s"){
		d1<-d1[ref1 %in% ref2,]
	}# if D keep tables as is

	similard1d2<-checkLines(d1,d2,refcol1,refcol2);

	return(invisible(similard1d2))
}
automatch<-function(){
	#### import data
	initd1d2<-importDfs();

	#### treat differences
	similard1d2<-checkSimStruct(initd1d2); # verifica la forma 
	corrected.d1d2<-matchSimilar(similard1d2); # verifica y corrige los valores

	#### export result
	cat(savingIn[[lang]],d1correctedname,andMes[[lang]],d2correctedname,"\n");
	cat(commonLinesSaved[[lang]],"\n\n")
	write.csv(corrected.d1d2$d1,d1correctedname,row.names=FALSE);
	write.csv(corrected.d1d2$d2,d2correctedname,row.names=FALSE);

	send<-"y";
	while(send=="y" & RMySQLok){
		send<-readline(paste(exportTableMySQL[[lang]],"[y/N]"));
		if(send=="y"){
			succes<-dbExportMySQL(corrected.d1d2$d1);
			if(succes){
				send<-"n";
en	}
		}
	}
	return(invisible(corrected.d1d2))
}
# automatch();

