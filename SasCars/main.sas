%let TARGET_VAR = MPG_City;/*MPG_city; /*MPG_highway;*/
%macro prepare_data(rowdata=sashelp.cars, traindata=train_cars, testdata=test_cars);

proc rank data=&rowdata out=cars_d groups=10;
where (type ne "Hybrid");
var &TARGET_VAR;
ranks R_&TARGET_VAR;
run;

proc sort data = cars_d;
by R_&TARGET_VAR;
run;

proc surveyselect data=cars_d out=&traindata n=25;
strata R_&TARGET_VAR;
run;

proc sql;
create table &testdata as select * from &rowdata
where not model in (select model from &traindata) and (type ne "Hybrid");
quit;

data &testdata;
set &testdata;
/*add here necessary variables generation*/
if (Cylinders eq .) then Cylinders=4;
run;

data &traindata;
set &traindata;
if (Cylinders eq .) then Cylinders=4;

/*add here necessary input variables generation*/
if (Type = "Hybrid") then Type2 = 0;
if (Type = "Sedan") then Type2 = 1;
if (Type = "Wagon") then Type2 = 2;
if (Type = "Sports") then Type2 = 3;
if (Type = "SUV") then Type2 = 4;
if (Type = "Truck") then Type2 = 5;

if (Origin = "Asia") then Origin2 = 0;
if (Origin = "Europe") then Origin2 = 1;
if (Origin = "USA") then Origin2 = 2;
	
WBQuad = WheelBase * WheelBase;
WQUAD = Weight * Weight;
HPQuad = HorsePower * HorsePower;
ESQuad = EngineSize * EngineSize;
CQuad = Cylinders * Cylinders;
IQuad = Invoice * Invoice;
LQuad = Length * Length;

WB_W = WheelBase * Weight;
HP_WB = HorsePower * WheelBase;
HP_W = HorsePower * Weight;
ES_WB = EngineSize * WheelBase;
ES_W = EngineSize * Weight;
ES_HP = EngineSize * HorsePower;
C_WB = Cylinders * WheelBase;
C_W = Cylinders * Weight;
C_HP = Cylinders * HorsePower;
C_ES = Cylinders * EngineSize;
I_WB = Invoice * WheelBase;
I_W = Invoice * Weight;
I_HP = Invoice * HorsePower;
I_ES = Invoice * EngineSize;
I_C = Invoice * Cylinders;
L_WB = Length * WheelBase;
L_W = Length * Weight;
L_HP = Length * HorsePower;
L_ES = Length * EngineSize;
L_C = Length * Cylinders;
L_I = Length * Invoice;
run;

%mend;

%prepare_data();

proc univariate data=train_cars;
	var &TARGET_VAR;
	histogram / 
	gamma(color=blue) normal(color=red) 
	lognormal(color=green) kernel(color=yellow)
	midpoints=10 to 60 by 2;
run;

proc reg data=train_cars;
	var &TARGET_VAR;
	model &TARGET_VAR = 
		Type2 Origin2 
		WheelBase Weight HorsePower EngineSize Cylinders
		Invoice Length;
run;

/* Наблюдаем распределение отклика с более тяжелым правым хвостом, также 
ненормальное распределение остатков.
Хоть и не удалось приблизить распределение гамма или логнормальным распределением
(по критериям соотвествия), делаем предположение, что распределение ближе к 
логнормальному, чем к нормальному.
Осуществляем преобразование отклика.
Также отбираем наиболее значимые переменные среди полиномиальных членов.*/


data train_cars_log;
	set train_cars;
	&TARGET_VAR = log(&TARGET_VAR + 1);
run;

proc reg data=train_cars_log plots=all;
	model &TARGET_VAR = 
		Type2 Origin2 
		WheelBase Weight HorsePower EngineSize Cylinders
		Invoice Length
		WBQuad WQuad HPQuad ESQuad CQuad
		WB_W HP_WB HP_W ES_WB ES_W ES_HP
		C_WB C_W C_HP C_ES
		I_WB I_W I_HP I_ES I_C
		L_WB L_W L_HP L_ES L_C L_I  / 
		selection=adjrsq;
run;

/* Наблюдаем распределение остатков более близкое к нормальному, уменьшение гетероскадастичности.
Также обращаем внивамние, что линейная зависимость хорошо приближает 
поведение преобразованного отклика.
Делаем вывод, что новая модель в целом применима.*/

/*--------------------------------------------------Отбор переменных и построение графика-------------------------------------------------*/

proc glmselect data=train_cars_log plots=all;
	model &TARGET_VAR = 
		Type2 Origin2 
		WheelBase Weight HorsePower EngineSize Cylinders
		Invoice Length
		WBQuad WQuad HPQuad ESQuad CQuad
		WB_W HP_WB HP_W ES_WB ES_W ES_HP
		C_WB C_W C_HP C_ES
		I_WB I_W I_HP I_ES I_C
		L_WB L_W L_HP L_ES L_C L_I
		/ selection=stepwise select=sl slstay=0.05 slentry=0.05 details=steps
		cvmethod=random(5);
run;

proc reg data=train_cars_log;
	model &TARGET_VAR = 
		Type2 
		Weight Horsepower EngineSize 
		WBQuad ESQuad CQuad 
		HP_W ES_HP C_WB C_W C_ES 
		I_WB I_HP L_C L_I;
		
	store mmodel;
	output out=train_cars_res pred=Result;
run;

/* Создание сетки */

/* Расчёт маскимума и минимума 2 основных переменных*/
data _null_;
	set train_cars_log end=last;
	retain max_W;
	retain max_HP;
	retain min_W;
	retain min_HP;
	
	if _N_ = 1 then do;
		max_W = 0.0;
		max_HP = 0.0;
		min_W = 1000000.0;
		min_Hp = 1000.0;
	end;
	
	if (Weight > max_W) then max_W = Weight;
	if (HorsePower > max_HP) then max_HP = HorsePower;
	if (Weight < min_W) then min_W = Weight;
	if (HorsePower < min_HP) then min_HP = HorsePower;
	
	if last then do;
		call symputx("max_W", max_W);
		call symputx("max_HP", max_HP);
		call symputx("min_W", min_W);
		call symputx("min_HP", min_HP);
	end;
run;

/* Равномерная сетка */
data grid (keep=Weight HorsePower);
	set train_cars_log;
	
	do i=1 to 20;
		Weight = ((&max_W - &min_W) * (i - 1)) / 20 + &min_W;
		do j=1 to 20;
			HorsePower = ((&max_HP - &min_HP) * (j - 1)) / 20 + &min_HP;
			output;
		end;
	end;
	stop;
run;

/* Расчёт средних для остальных переменных, для категориальных - мода */
proc means data=train_cars_log;
	output out=means;
run;

data mean_only;
	set means;
	drop _TYPE_ _FREQ_ _STAT_ Weight HorsePower Type2 Origin2;
	where (_STAT_ = "MEAN");
run;

proc univariate data=train_cars_log modes;
	var Type2 Origin2;
	output out=cat_modes mode=Type2 Origin2;
run;

data means_full;
	merge mean_only cat_modes;
run;

data grid_mean;
	set means_full;
	do i=1 to 400;
		output;
	end;
run;

/* Слияние полученных сеток */
data grid_full;
	merge grid grid_mean;
run;

/* Расчёт предсказаний */
proc plm source=mmodel;
	score data=grid_full out=res pred=Result;
run;

data _null_;
	set train_cars_res end=last;
	MSE + (Result - &TARGET_VAR) ** 2;
	if last then call symputx("MSE", MSE / _N_);
run;

/* Построение графика */
data axes(keep=Weight HorsePower Result);
	set res;
	Result = exp(Result + &MSE / 2) - 1;
run;

options papersize="ISO A5" orientation=landscape;
ods pdf file="~/3/plot.pdf";
proc g3d data=axes;
   plot Weight * HorsePower = Result / rotate=-60;
run;
ods pdf close;