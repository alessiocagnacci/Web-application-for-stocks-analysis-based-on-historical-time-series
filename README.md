# App-per-analizzare-titolo-azionario-con-serie-storiche-in-R
*Applicazione web che permette di scegliere titolo da Yahoo e periodo di analisi e restituisce un analisi attraverso lo studio di ARIMA.*

Ho deciso di creare un app per rendere più facile la modifica del ticker su cui cercare le analisi, precedente svolte nella requisitory "Analisi di un titolo azionario usando modelli ARIMA". 

Ho usato la libreria shiny per creare l'applicazione, lubridate per facilitare la conversione delle date future e zoo per risolvere un problema dato dall'utilzzo dei dati mensili.

Ho pure aggiunto un eveluzione che utilizza oltre al modello ARIMA il metodo ETS in modo da avere un analisi più accurata.
