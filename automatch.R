######################
#### parametros
######################

d1correctedname<-"d1corrected.csv"
d2correctedname<-"d2corrected.csv"

######################
#### functiones
######################
RMySQLok<-require("RMySQL")
graphok<-require(gWidgetstcltk);
cat("\nSi hay error antes de esta linea, no apurarse y continuar\n")
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
		stop("columnas solo en:\n d1:",solo1,"\n d2:",solo2);
	}else{
		# reorder columns if needed so that d2 has the same order than d1
		d2<-d2[,match(names(d1),names(d2))]
	}
	return();
}
## identifica referencias
getRefCol<-function(db){
	# return the columns number as chosen by user
	cat("Escoger las columnas que van a servir de referencia:\n");
	print(names(db));
	refcoltxt<-readline("Entrar num columna separados con coma:[1,2] ");
	refcol<-suppressWarnings(as.numeric(strsplit(refcoltxt,",")[[1]]));
	if(refcoltxt==""){
		refcol<-c(1,2);
	}
	if(length(which(is.na(refcol)))>0){
		cat("No entiendo:",refcoltxt,"\n");
		continue<-readline("Intentar otra vez?[Y/n] ");
		if(continue=="n"){
			stop("Bye!");
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
		cat("Siguientes duplicados en 1:\n",ref1[dup1]);
		cat("\n\nPor favor escoger mejor los campos de referencia o\neliminar estos duplicados\n");
		matchok<-FALSE
	}
	if(length(dup2)>0){
		cat("\nSiguientes duplicados en 2:\n",ref2[dup2]);
		cat("\n\nPor favor escoger mejor los campos de referencia o\neliminar estos duplicados\n o ");
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
		cat("\nNo hay el mismo numero de lineas:\nd1:",dim(d1)[1],"\nd2:",dim(d2)[1]);
		cat("\nIntento identificar las lineas que faltan.");
		linesok<-FALSE
	}
	# verifica duplicados para referencias
	linesok<-min(checkDuplicates(ref1,ref2),linesok);

	# identifia las linias solo en una de las bases
	solo1<-setdiff(ref1,ref2);
	solo2<-setdiff(ref2,ref1);
	if(length(solo1)>0){
		cat("\n-lineas solo en 1:\n",solo1,"\n");
		rm1<-which(ref1 %in% solo1); # remueve los que solo estan en 1
		d1<-d1[-rm1,];
		linesok<-FALSE
	}
	if(length(solo2)>0){
		cat("\n-lineas solo en 2:\n",solo2,"\n");
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
	checkColumns(d1,d2);
	refcol<-getRefCol(d1);
	cat("Utilizando:",refcol,"\n");

	similard1d2<-checkLines(d1,d2,refcol,refcol);

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

	cat("Hay:", sum(errorperlinia),"errores en",length(errorlinias),"linias\n");

	# offer correction
	correc<-list();
	count<-1;
	while(count<=length(errorlinias)){
		linia <- errorlinias[count] 
		cat("Error",count,"de",length(errorlinias),"\n");
		# correct a copy
		difcol<-c(refcol,which(dif[linia,]==TRUE));
		original1<<-cbind(d1[linia,difcol]);
		original2<<-cbind(d2[linia,difcol]);
		original<-rbind(original1,original2);
		correcto<-edit(original);
		# ofrece de parar si no hay correcciones
		if( isTRUE(all.equal(correcto,original,check.attributes=FALSE))){
		    continue<-readline("No se corrigio nada. Usted quiere:
	s: stop. Parar la correccion (se va a salvar el estado corriente)?
	r: regresar. Corregir de nuevo este error.
	C: continuar.\n 
	")
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
		continue<-readline("\n Hay problemas, hacer el match con parte commun de las bases?[y/N]");
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
		cat("Hay",sum(dif),"errores. Analysar de nuevo?[y/N]\n");
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
		cat("Escoge perfil:\n");
		print(linesgroup);
		# numgroup<-1
		numgroup<-readline("Numero (nada permite entrar nuevo): ");
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
		db<-readline("Base de datos?[] ");

			if(.Platform$OS.type=="unix"){
				user<-system("whoami",intern=TRUE);
			}else{
				user<-"";
			}

		cat("Usuario [",user,"]:",sep="");
		input<-readline("");
		if(input!=""){
			user<-input
		}	
		password<-readline("Contrasenia?[] ");
		con<-try(dbConnect(sqldriver,user=user,password=password,dbname=db,host=host));
	}
	if(class(con)!="try-error"){
		cat("Conneccion ok\n");
			if(!oldgroup){
			savegroup<-readline("Guardar la connection?[y/N] ");
			if(savegroup=="y"){
				newgroup<-readline("Nombre connection? ");
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
	tabla<-readline("Nombre tabla en MySQL? ");
	
	if(dbExistsTable(con,tabla)){
		ready<-FALSE;
		while(! ready){
			mode<-readline("Quiere remplazar todo, actualizar o aniadir?[r/z/a/q] ");
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
				cat("Tabla no enviada\n");
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
		cat("Tabla exportada con exito\n");
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
			numtabla<-as.integer(readline("Numero de la tabla? "));
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
		origine<-readline("Importar csv o de mysql?[c/M] ");
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
	cat("Origen de la tabla?\n")
	df<-dbImportMySQL()
	if(struct){
		df<-df[1,]
	}
	cat("\nDestinaccion de la tabla?\n")
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
	cat("En la primera tabla.")
	refcol1<-getRefCol(d1); # cual columnas usar para el merge
	cat("En la secunda tabla.")
	refcol2<-getRefCol(d2); # cual columnas usar para el merge

	ref1<-getref(d1,refcol1); 
	ref2<-getref(d2,refcol2);

	cat("Sacar los que faltan para Primera tabla, Secunda o los Dos?[p/s/D]")
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
	cat("Salva las bases corregidas en:",d1correctedname,"y",d2correctedname,"\n");
	cat("Nota: solo las lineas en ambas tablas son salvadas\n\n")
	write.csv(corrected.d1d2$d1,d1correctedname,row.names=FALSE);
	write.csv(corrected.d1d2$d2,d2correctedname,row.names=FALSE);

	send<-"y";
	while(send=="y" & RMySQLok){
		send<-readline("Exportar la tabla a MySQL?[y/N]");
		if(send=="y"){
			succes<-dbExportMySQL(corrected.d1d2$d1);
			if(succes){
				send<-"n";
			}
		}
	}
	return(invisible(corrected.d1d2))
}
# automatch();

