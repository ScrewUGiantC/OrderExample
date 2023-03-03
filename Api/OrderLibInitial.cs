using System;

// using System.Threading.Tasks;

////////// for Debug and MessageBox ///////////////////////////////////////


namespace OrderExample.Api
{
    public class OrderLibInitial
    {
        public OrderLibInitial()
        {
            // 電子單號起始值
            // 
            string str_hhmmss = DateTime.Now.ToString("HHmmss", System.Globalization.DateTimeFormatInfo.InvariantInfo);
            OrderLibGlobal.reqId = Convert.ToInt32(str_hhmmss) * 10000; 

            // Order Channel 
            // 
            OrderLibGlobal.ConnData.OrderSvr.lineID = "line001"; // lineID 決定了 config 檔名稱 line001.ini 

            // Push Channel 
            // 
            // Global.ConnData.PushSvr.lineID = "line001"; // lineID 決定了 config 檔名稱 line001.ini  
 
        } // Initial()




    } // class Initial 
}
