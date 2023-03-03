using System;
using System.Collections.Generic;
using System.IO;

namespace OrderExample.Api
{
    internal class OrderLibGlobal
    { 
        // 測試用途. 使用很大的 array 存 委回字串. 
        public static List<string> list_OrderReply = new List<string>();
        public static Object list_OrderReply_lock = new Object();


        /// <summary>
        /// /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        /// </summary>

        public static int reqId = 0;  // int_seqno = 0;	// 電子單號
		
		
        // 連線資料 -- Note: 在此, 只是仿 元大的作法. 也許, 如果本 AP 有多條 下單連線, 則 可能 底下 不能使用 static data member !!! 
        // 
        public class ConnData
        {
            public class OrderSvr
            { 	
                public static IntPtr ttCliHdl = IntPtr.Zero;		// Client Handler.  8 byte at least for both WIN32 and WIN64. 
                public static string basePath = Path.GetFullPath(@".\\tbase");	// FIX 連線的設定檔路徑 ( 絕對路徑 )
                public static bool IsConnected = false;	// 是否"應該"處於連線的狀態
				public static int dll_testCli_init_flg = 0; 
				
                public static string lineID = ""; 	// lineID 決定了 config 檔名稱 line001.ini  在 Initial.cs Initial() 裡面 設定 lineID. 
                public static string TradeIP = "61.216.152.219";
                public static int TradePort = 62001;
				public static int BkSvrPort = 62313; 	// 回補 Server Port
				// public static string UId = "";		// 連線 user ID 
				// public static string Pwd = ""; 		// 連線 password 
		

				// delegate
				// 
                // public static ConnTronSvr.onRtnReply_func_t onRtnReply_func;  
				// 			取得 delegate: Global.ConnData.PushSvr.onRtnReply_func = new ConnTronSvr.onRtnReply_func_t(OrderMsg_CallBack); 
				//	                       OrderMsg_CallBack 的 function prototype 為 onRtnReply_func_t
                // public static IntPtr ap_param_onRtnReply = IntPtr.Zero;
                // 
				public static OrderLibConnTronSvr.tseCli_onLogonReplyFunc_t onLogonReplyFunc;
		
				public static OrderLibConnTronSvr.tseCli_onDisconnFunc_t onDisconnFunc;
        
				public static OrderLibConnTronSvr.tseCli_onRejectWhenDisconnFunc_t onRejectWhenDisconnFunc;
		
				public static OrderLibConnTronSvr.tseCli_onAlterPwdReplyFunc_t onAlterPwdReplyFunc;
		
				public static OrderLibConnTronSvr.tseCli_onRecvOrderReplyFunc_t onRecvOrderReplyFunc; 
	
				public static OrderLibConnTronSvr.tseCli_onRecvMatchReplyFunc_t onRecvMatchReplyFunc;
		
				///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
				//
				// 華南測試時 查詢風控部位 機制
				//
				public static OrderLibConnTronSvr.tseCli_onQryMoneyReplyFunc_t onQryMoneyReplyFunc;
				
				public static OrderLibConnTronSvr.tseCli_onQryStockPosReplyFunc_t onQryStockPosReplyFunc;
				

				//////////////////////////////
				// 委託&回報 回補 機制    
				//
				public static OrderLibConnTronSvr.tseCli_reRpt_onSysStatFunc_t reRpt_onSysStatFunc; 

				public static OrderLibConnTronSvr.tseCli_reRpt_onRecvOrderReplyFunc_t reRpt_onRecvOrderReplyFunc; 

				public static OrderLibConnTronSvr.tseCli_reRpt_onRecvMatchReplyFunc_t reRpt_onRecvMatchReplyFunc;

				public static OrderLibConnTronSvr.tseCli_reRpt_onRecvOrderFunc_t reRpt_onRecvOrderFunc;




                public static OrderLibConnTronSvr ConnTronSvr = new OrderLibConnTronSvr(OrderLibConnTronSvr.enumType.Order);
            }
			

			/******** 
            public class PushSvr
            {
                public static bool IsConnected = false;	// 是否"應該"處於連線的狀態
                public static IntPtr ttCliHdl = IntPtr.Zero;		// Client Handler.  8 byte at least for both WIN32 and WIN64. 
                public static string basePath = Path.GetFullPath(@".\\tbase");	// 連線的設定檔路徑 ( 絕對路徑 )

                public static string lineID = ""; // lineID 決定了 config 檔名稱 line001.ini  在 Initial.cs Initial() 裡面 設定 lineID. 
                public static string PushIP = "";
                public static int PushPort = 0;

                public static ConnTronSvr.onRtnReply_func_t onRtnReply_func;
                public static IntPtr ap_param_onRtnReply = IntPtr.Zero;

                public static ConnTronSvr ConnTronSvr = new ConnTronSvr(ConnTronSvr.enumType.Push);
            }
			**********/
			
			
        } // class ConnData

    } // class Global 

}
