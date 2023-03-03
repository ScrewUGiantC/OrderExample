using System;
using System.Diagnostics;
using System.Runtime.InteropServices;

namespace OrderExample.Api
{
    public class OrderLibConnTronSvr
    {
        public const uint ORDER_MARK = 16222001 ;

		public const int EXECID_LEN = 12; 
		public const int TSE_ID_LEN = 16; 
		public const int ClOrdID_LEN = 6; 
		
		public const int ClientIP_LEN = 15; 
		public const int SALENO_LEN = 4; 
		public const int UDD_LEN = 32; 
		
		
        public enum enumType { Order, Push };
        private enumType _Type;
		
		

        #region Import from dll_tseCli.dll 
      

        #region Struct

        // broker-defined data
        //
        [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Ansi)]
        public struct broker_def_t
        {
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = ClientIP_LEN)]
			public byte[] ClientIP;			// IP of Client(必填)
			
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = SALENO_LEN)]
			public byte[] SALENO;			// 營業員編號 (委成回報時填入)
			
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = UDD_LEN)]
			public byte[] UDD;					// user-defined 欄位. 
		}
		
		
        // tsecli_order_t       -- 委託單
        //
        [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Ansi)]
        public struct tsecli_order_t
        {
		//// common header of all client messages
			public UInt32 mark;                 	// client 填入 TSECLI_ORDER_MARK        Little Endian
			public UInt32 reqId;                    // client 填入 電子單編號, Request ID   Little Endian
			public UInt64 SysUNo;					// 由 library 系統 自動填寫. AP 不用填寫.
			
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = OrderLibConnTronSvr.TSE_ID_LEN)]
			public byte[] SysUId;			// 由 library 系統 自動填寫. AP 不用填寫.
		//// 

			public broker_def_t broker_def;	// 券商自行定義的 額外欄位. 用於 委託單, 回報單. broker_def is only set in NewOrder, 異動單 不會變更 broker_def.
				
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = OrderLibConnTronSvr.ClOrdID_LEN)]
			public byte[] ClOrdID;			// 由 OMS 填寫. 下單 client 不用填. 
		////
			/* 子系統類別 (1-普通股) PIC X(01)				*/
			public char subsys_name;

			/* 委託功能別 (1-買，2-賣，3-減量，4-取消，6-改價) PIC X(01)	*/
			public char function_code;

			/* 券商代號 	PIC X(04)											*/
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
			public byte[] broker_id; 

			/* 委託書編號 (委託時為空白，改量、取消時為原委託書編號) PIC X(05)	*/
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 5)]
			public byte[] order_no; 

			/* 客戶帳號 (含檢碼) 	PIC 9(07)									*/
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 7)]
			public byte[] cust_no; 

			/* 股票代號 	PIC X(06)											*/
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 6)]
			public byte[] stock_no; 

			/*   1:委託種類 (0-現股、普通，1-代辦融資，2-代辦融券，3-自辦融資，4-自辦融券
			 *                           5: 借券 5 (一般借券, 平盤以下不可賣 (只能以比開盤價更高的價格賣出))
										 6: 借券 6 (策略性的特殊借券, 平盤以下可賣. 
										 7: 資 7 (當沖)
										 8: 券 8 (當沖) )
				   ORDER-TYPE          PIC X(01).
			
			   目前 大戶下單 只提供 0-現股、普通，3-自辦融資，4-自辦融券，7: 資 7 (當沖)，8: 券 8 (當沖)
			   改單 刪單 查詢 時, 填空白. 										*/
			public char order_type; 

			/* 委託單價x100 	
				 限價或改價之買賣單填價格 x 100. 譬如 60.65 元, 則填 6065.
				 減量 刪單 時, 填 0.						 
				 市價單時, 填 0. 											*/
			public UInt32 order_price100; 

			/* 委託數量 (普通、盤後定價-張數，零股-股數) PIC 9(04)
					減量時, 整股填欲減少之張數. 
					減量時, 零股填更改後之委託股數. 
					刪單 改價 查詢 時, 填 0.											*/
			public UInt16 order_qty; 
	
			/* PRICE-TYPE	X(1)  1 –市價 (僅適用於逐筆交易時段) 2 –限價 		
								  改單 刪單 查詢 時, 填空白. 
			*/
			public char price_type;
	
			/* TIME-IN-FORCE	X(1)  0 –ROD當日有效  3 –IOC(立即成交否則取消) 4 –FOK(立即全部成交否則取消) 註：IOC、FOK僅適用於逐筆交易時段 	
									  改單 刪單 查詢 時, 填空白. 
			*/
			public char time_in_force;
	
        }


        // tseOrderReply_t         -- 委託回報
        //
        [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Ansi)]
        public struct tseOrderReply_t 
        {
			public broker_def_t broker_def;	// 券商自行定義的 額外欄位. 用於 委託單, 回報單.  
			 
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = OrderLibConnTronSvr.EXECID_LEN)]
			public byte[] ExecID;             	// 委回序號的 unique key. 證交所填 ClOrdID
		
		////

			public char subsys_name;		// 子系統類別 (1-普通股) 		
			public char function_code;		// 委託功能別 (1-買，2-賣，3-減量，4-取消，6-改價)		 
											// FIX 回報無法區分是否是刪單失敗 或是 改單失敗. OMS 沒有存刪改單的 order record. 先假設是刪單失敗. 目前大機不支援減量單. 
			public char message_type;		// 1: 委託回報
			
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 9)]
			public byte[] message_time;		// 訊息傳出時間 hhmmssnnn. nnn: 千分之1秒.

		////////////////////////////////////////////

			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
			public byte[] status_code;		// FIX 主機連線, 交易所傳回的錯誤訊息代碼. 9999: rejected by OMS. 
											// 當 status_code (早盤 0000 OK, 0031 警告 或 0032 警告 或 0051 警告), 才算是委託OK.
											// 0031: 外資買進或借券賣委託數量被刪減 
											// 0032: 取消數量超過原有數量
											// 0051: 委託觸及價格穩定措施上、下限價格，市價、IOC委託可成交部分之委託數量生效，剩餘委託數量剔退
											// 9999: 大機自訂的 status code, 代表 被 大機 reject 的委回
											// 當 status code 為 0000, 0031, 0032, 0051 時, 才可利用 before_qty 與 after_qty 來判斷 委託單 實際造成的數量變更. 
											// 其他的 status_code定義, 請查 交易所 FIX 主機連線的spec. 

			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
			public byte[] broker_id;		// 券商代號
			
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 2)]
			public byte[] pvc_id;			// PVC代號. socket ID. 如果是證交所傳送訊息給證券商，TargetCompID(56) 為連線證券商的(市場別+券商代號＋FIX Socket ID)。
											// 如果是 OMS reject 的委回, 則 pvc_id 填空白.

			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 5)]
			public byte[] order_no;			// 委託書編號. 如果是 OMS reject 的委回, 則填空白.
			
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 7)]
			public byte[] cust_no;			// 客戶帳號含檢碼. 
 
			public char ivacno_flag;		// 客戶下單類別註記
			
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 6)]
			public byte[] stock_no;			// 股票代號
			
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 9)]
			public byte[] order_price;		// 委託單價 5位整數+4位小數
	
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 6)]
			public byte[] order_qty;		// 委託數量(普通、盤後定價-張數，零股-股數)
	
			public char buy_sell_code;		// 買賣別 (B-買進, S-賣出)
			public char exch_code;			// 交易型態 (0-普通, 2-零股)

			/*   1:委託種類 (0-現股、普通，1-代辦融資，2-代辦融券，3-自辦融資，4-自辦融券
										 5: 借券 5 (一般借券, 平盤以下不可賣 (只能以比開盤價更高的價格賣出))
										 6: 借券 6 (策略性的特殊借券, 平盤以下可賣.
										 7: 資 7 (當沖)
										 8: 券 8 (當沖) )		*/
			public char order_type;	

			/* Price Type '1': 市價, '2': 限價 */
			public char price_type;

			/* 委託有效期間. '0' 當日有效、'3' IOC、'4' FOK */
			public char time_in_force;
	
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
			public byte[] order_date;		// 日期 (MMDD)	PIC 9(04) 月日
			
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 9)]
			public byte[] order_time;		// 交易所回報時間 hhmmssnnn. nnn: 千分之1秒. 如果是 OMS reject 的委回, 則填 OMS 產生委回的時間.

			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 6)]
			public byte[] before_qty;		// 委託前數量(不含已成交量). 當 status_code (早盤 0000, 0031, 0032, 0051) 時, 才有 正確的 value. 
											// 大機只支援早盤

			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 6)]
			public byte[] after_qty;		// 委託後數量(不含已成交量). 當 status_code (早盤 0000, 0031, 0032, 0051) 時, 才有 正確的 value. 
											// 大機只支援早盤 
//
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 6)]
			public byte[] original_orderqty;	// 原委託單的委託張數  Max 6 digits

			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 9)]
			public byte[] before_price;		// 改價前的委託價格  5位整數+4位小數, 元富盤中委回使用. 改價時, 此欄位才有效. 

			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 9)]
			public byte[] bull_price;		// 市價單的漲停價格  5位整數+4位小數, 元富盤中委回使用. 市價單時, 此欄位才有效. 

			public char unsolicitedFlag;	// 主動取消市價單註記，tag 150 = 'D' && tag 378 = '8'，條件成立:'Y'  Default:'N'，元富盤中委回使用

        }


        // tseMatchReply_t     -- 成交回報
        [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Ansi)]
        public struct tseMatchReply_t
        {
			public broker_def_t broker_def;	// 券商自行定義的 額外欄位. 用於 委託單, 回報單.  
			
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = OrderLibConnTronSvr.EXECID_LEN)]
			public byte[] ExecID;	// 成回序號的 unique key: Side + Market trx no.  Side -- '1':買, '2': 賣.  Market trx no 應該是 成交總檔編號. 

		////

			public char subsys_name;		// 子系統類別 (5-成交回報子系統) 		
			public char function_code;		// 委託功能別 : 0		
			public char message_type;		// 0 
			
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 9)]
			public byte[] message_time;		// 訊息傳出時間 hhmmssnnn. nnn: 千分之1秒.

////////////////////////////////////////////

			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 6)]
			public byte[] stock_no;			// 股票代號
			
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 8)]
			public byte[] mth_qty;			// 成交數量 PIC 9(08) (普通、盤後定價-張數，零股-股數) 
			
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 9)]
			public byte[] mth_price;			// 成交單價 5位整數+4位小數
			
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 9)]
			public byte[] mth_time;			// 成交時間 hhmmssnnn. nnn: 千分之1秒.
			
			public char exch_code;			// 交易型態 (0-普通, 2-零股)
			public char buy_sell_code;		// 買賣別 (B-買進, S-賣出)
	
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 5)]
			public byte[] order_no;			// 委託書編號
	
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 7)]
			public byte[] cust_no;			// 客戶帳號含檢碼 

			/*   1:委託種類 (0-現股、普通，1-代辦融資，2-代辦融券，3-自辦融資，4-自辦融券
										 5: 借券 5 (一般借券, 平盤以下不可賣 (只能以比開盤價更高的價格賣出))
										 6: 借券 6 (策略性的特殊借券, 平盤以下可賣.
										 7: 資 7 (當沖)
										 8: 券 8 (當沖) )		*/
			public char order_type;

			/* Price Type '1': 市價, '2': 限價 */
			public char price_type;

			/* 委託有效期間. '0' 當日有效、'3' IOC、'4' FOK */
			public char time_in_force;
	
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
			public byte[] broker_id;		// 券商代號
        }


		///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//
		// 華南測試時 查詢風控部位 機制
		//

		// apQryMoney_t         -- 查詢風控錢的部位. 只是做為 C# AP 轉換資料傳給 C DLL 的用途, working area. 此 data structure 不是 C DLL API 的參數. 
        //
        [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Ansi)]
        public struct apQryMoney_t
        {
			/* 券商代號 	PIC X(04)											*/
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
			public byte[] broker_id; 

			/* 客戶帳號 (含檢碼) 	PIC 9(07)									*/
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 7)]
			public byte[] cust_no; 

        }
		
		
        // apQryStockPos_t         -- 查詢風控庫存的部位. 只是做為 C# AP 轉換資料傳給 C DLL 的用途, working area. 此 data structure 不是 C DLL API 的參數. 
        //
        [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Ansi)]
        public struct apQryStockPos_t
        {
			/* 券商代號 	PIC X(04)											*/
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
			public byte[] broker_id; 

			/* 客戶帳號 (含檢碼) 	PIC 9(07)									*/
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 7)]
			public byte[] cust_no; 

			/* 股票代號 	PIC X(06)											*/
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 6)]
			public byte[] stock_no; 
	
        }
	
		
		
        // tseQryMoneyReply_t         -- 查詢風控錢的部位所得到的回報
        //
        [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Ansi)]
        public struct tseQryMoneyReply_t 
        {
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
			public byte[] broker_id;		// 券商代號
			
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 7)]
			public byte[] cust_no;			// 客戶帳號含檢碼. 
			
			public Int64 TotTradeAmt;			// 投資能力 -- 來自券商客戶主檔
			public Int64 UsedTradeAmt; 			// 已使用的委託價金
	
			public Int64 MSTotTradeAmt;			// 處置股票投資能力.  MS: Measuring Stock (處置股票). 
			public Int64 MSUsedTradeAmt; 		// 已使用的處置股票委託價金.  MS: Measuring Stock (處置股票).
	
        }

		// tseQryStockPosReply_t         -- 查詢風控股票庫存所得到的回報
        //
        [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Ansi)]
        public struct tseQryStockPosReply_t 
        {
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
			public byte[] broker_id;		// 券商代號
			
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 7)]
			public byte[] cust_no;			// 客戶帳號含檢碼. 
			
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = 6)]
			public byte[] stock_no;			// 股票代號
			
			
			public Int64 RemainQty;				// 集保庫存 "張數"
			public Int64 ControlQty;			// 集保控管 "張數"

			public Int64 SoldQty;				// 委賣昨日庫存的 "張數"

			public UInt32 TodayBuyQty;			// 今日庫存.  今日現股當沖先買成交 "張數", 給可當沖股票使用
			public UInt32 TodaySoldQty;			// 委賣今日庫存與券源 "張數", 給可當沖股票使用			
			
        }

        #endregion


/*  Function Sequence

	IntPtr p_tseCli; 

    dll_tseCli_init( p_tseCli );   // C#:  IntPtr ref p_tseCli. 

 	dll_tseCli_set_onLogonReply( p_tseCli, ap_onLogonReply, onLogonReply_ap_param );  // C#:  IntPtr ref p_tseCli
      ... 
		call some "set" callback functions. 
	  ... 
	dll_tseCli_set_onRecvOrderReply( p_tseCli, ap_onRecvOrderReply, onRecvOrderReply_ap_param );  // C#:  IntPtr ref p_tseCli
	dll_tseCli_set_onRecvMatchReply( p_tseCli, ap_onRecvMatchReply, onRecvMatchReply_ap_param );  // C#:  IntPtr ref p_tseCli


	dll_tseCli_open( p_tseCli, ... );    // C#:  IntPtr ref p_tseCli. Open Connection. DLL 底層自動斷線重連.
	
		Loop
		{
			dll_tseCli_send_order( p_tseCli, ...);     
			... 
		}


	dll_tseCli_close( p_tseCli );   // C#:  IntPtr ref p_tseCli.  Close Connection. 

	dll_tseCli_term( p_tseCli );   // C#:  IntPtr ref p_tseCli

*/

		#region Callback Delegate
		
		[UnmanagedFunctionPointer(CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        public delegate void tseCli_onLogonReplyFunc_t( IntPtr ap_param, UInt32 iRC );
		
		[UnmanagedFunctionPointer(CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        public delegate void tseCli_onDisconnFunc_t( IntPtr ap_param );
        
		[UnmanagedFunctionPointer(CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        public delegate void tseCli_onRejectWhenDisconnFunc_t( IntPtr ap_param, UInt32 reqId );
		
		[UnmanagedFunctionPointer(CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        public delegate void tseCli_onAlterPwdReplyFunc_t( IntPtr ap_param, UInt32 reqId, UInt32 iRC );
		
		[UnmanagedFunctionPointer(CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        public delegate void tseCli_onRecvOrderReplyFunc_t( IntPtr ap_param, UInt32 reqId, UInt32 iRC, ref tseOrderReply_t p_tseOrderReply, ref broker_def_t broker_def ); 
	
		[UnmanagedFunctionPointer(CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        public delegate void tseCli_onRecvMatchReplyFunc_t( IntPtr ap_param, UInt32 reqId, ref tseMatchReply_t p_tseMatchReply, ref broker_def_t broker_def );
		
		
		///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//
		// 華南測試時 查詢風控部位 機制
		//

		[UnmanagedFunctionPointer(CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        public delegate void tseCli_onQryMoneyReplyFunc_t( IntPtr ap_param, UInt32 reqId, UInt32 iRC, ref tseQryMoneyReply_t p_Reply  );
		
		[UnmanagedFunctionPointer(CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        public delegate void tseCli_onQryStockPosReplyFunc_t( IntPtr ap_param, UInt32 reqId, UInt32 iRC, ref tseQryStockPosReply_t p_Reply  );
		


		//////////////////////////////
		// 委託&回報 回補 機制    
		//

		/*
			void *ap_param

			int SysStat				0: 回補的連線 disconnected   
									1: 回補的連線 connected 
									2: 回補完畢
		*/
		[UnmanagedFunctionPointer(CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        public delegate void tseCli_reRpt_onSysStatFunc_t( IntPtr ap_param, int SysStat ); 


		[UnmanagedFunctionPointer(CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        public delegate void tseCli_reRpt_onRecvOrderReplyFunc_t( IntPtr ap_param, UInt32 reqId, UInt32 iRC, ref tseOrderReply_t p_tseOrderReply, ref broker_def_t broker_def ); 

 
		[UnmanagedFunctionPointer(CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        public delegate void tseCli_reRpt_onRecvMatchReplyFunc_t( IntPtr ap_param, UInt32 reqId, ref tseMatchReply_t p_tseMatchReply, ref broker_def_t broker_def );


		[UnmanagedFunctionPointer(CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        public delegate void tseCli_reRpt_onRecvOrderFunc_t( IntPtr ap_param, UInt32 reqId, ref tsecli_order_t p_tsecli_order, ref broker_def_t broker_def ); 


        #endregion  // Callback Delegate
		
		
		// allocate tseCli handle 
		//
		[DllImport("dll_tseCli.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern int dll_tseCli_init( ref IntPtr p_tseCliHdl );

		// release tseCli handle 
		//
		[DllImport("dll_tseCli.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)] 
		public static extern void dll_tseCli_term( ref IntPtr p_tseCliHdl ); 


		/*  Open Connection
			DLL 底層自動斷線重連.
			
			Input
				p_tseCliHdl	allocated by calling dll_tseCli_init() in advance. 
				SvrIP
				iSvrPort
				LogPath				在 LogPath 之下 會自動產生 日期子目錄 YYYY-MM-DD. 在 日期子目錄 底下 產生 log file. 
				UId					User ID for logon
				Pwd					Password for logon 
				sendCoreId 			not used for Windows platform. the CPU core ID bound for the sending thread of the client. 
									if sendCoreId < 0, we dont' bind core. 
			Return
				>= 0	OK
				< 0		error 
		*/
		[DllImport("dll_tseCli.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)] 
		public static extern int dll_tseCli_open( ref IntPtr p_tseCliHdl, string SvrIP, int iSvrPort, string LogPath, string UId, string Pwd, int sendCoreId );
		
		
		// Close Connection
		//
		[DllImport("dll_tseCli.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern void dll_tseCli_close( ref IntPtr p_tseCliHdl );
		
		
		// Note: 底層 自動斷線重連並且重新登入, 不須由 AP 去重連. 
		//       重新登入後, onLogonReply 又會被 call 1 次. 
		[DllImport("dll_tseCli.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern void dll_tseCli_set_onLogonReply(ref IntPtr p_tseCliHdl, tseCli_onLogonReplyFunc_t ap_onLogonReply, IntPtr onLogonReply_ap_param );
		
		
		// Note: 底層 自動斷線重連並且重新登入, 不須由 AP 去重連, 不用 call dll_tseCli_open
		// 
		[DllImport("dll_tseCli.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern void dll_tseCli_set_onDisconnected( ref IntPtr p_tseCliHdl, tseCli_onDisconnFunc_t ap_onDisconn, IntPtr onDisconn_ap_param );


		[DllImport("dll_tseCli.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern void dll_tseCli_set_onRejectWhenDisconn( ref IntPtr p_tseCliHdl, tseCli_onRejectWhenDisconnFunc_t ap_onRejectWhenDisconn, IntPtr onRejectWhenDisconn_ap_param );
		
		[DllImport("dll_tseCli.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern void dll_tseCli_set_onAlterPwdReply( ref IntPtr p_tseCliHdl, tseCli_onAlterPwdReplyFunc_t ap_onAlterPwdReply, IntPtr onAlterPwdReply_ap_param );

		[DllImport("dll_tseCli.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern void dll_tseCli_set_onRecvOrderReply( ref IntPtr p_tseCliHdl, tseCli_onRecvOrderReplyFunc_t ap_onRecvOrderReply, IntPtr onRecvOrderReply_ap_param );
		
		[DllImport("dll_tseCli.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern void dll_tseCli_set_onRecvMatchReply( ref IntPtr p_tseCliHdl, tseCli_onRecvMatchReplyFunc_t ap_onRecvMatchReply, IntPtr onRecvMatchReply_ap_param );



		///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//
		// 華南測試時 查詢風控部位 機制
		//

		[DllImport("dll_tseCli.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern void dll_tseCli_set_onQryMoneyReply( ref IntPtr p_tseCliHdl, tseCli_onQryMoneyReplyFunc_t ap_onQryMoneyReply, IntPtr onQryMoneyReply_ap_param );
		
		
		[DllImport("dll_tseCli.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern void dll_tseCli_set_onQryStockPosReply( ref IntPtr p_tseCliHdl, tseCli_onQryStockPosReplyFunc_t ap_onQryStockReply, IntPtr onQryStockReply_ap_param );
		

	
		/*
			Input

				tsecli_order_t  p_tsecli_order				

			Return
				< 0		error
				>= 0	OK		
		*/
		[DllImport("dll_tseCli.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern int dll_tseCli_send_order( ref IntPtr p_tseCliHdl, ref tsecli_order_t p_tsecli_order );


		///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//
		// User Login Password 機制
		//

		/*	
			Input
				reqId				client AP 自行編訂的 unique request ID. client AP 做 Async request and reply 之 識別用途. 
				pszOldPwd			up to 32 bytes. null-terminated string
				pszNewPwd			up to 32 bytes. null-terminated string
			Return
				>= 0		OK
				< 0			error 
		*/
		[DllImport("dll_tseCli.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern int dll_tseCli_alterPwd( ref IntPtr p_tseCliHdl, UInt32 reqId, string pszOldPwd, string pszNewPwd  );




		///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//
		// 華南測試時 查詢風控部位 機制
		//

		/*	查詢風控裡面錢的部位

			Input
				i32	reqId				client AP 自行編訂的 unique request ID. client AP 做 Async request and reply 之 識別用途. 
				char *p_broker_id		4 bytes. not NULL terminated. 
				char *p_cust_no			7 bytes. not NULL terminated.  
			Return
				>= 0		OK
				< 0			error 
		*/
		[DllImport("dll_tseCli.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
				public static extern int dll_tseCli_QryMoney( ref IntPtr p_tseCliHdl, UInt32 reqId, string p_broker_id, string p_cust_no  );



		/*	查詢風控裡面股票庫存的部位

			Input
				i32	reqId				client AP 自行編訂的 unique request ID. client AP 做 Async request and reply 之 識別用途. 
				char *p_broker_id		4 bytes. not null-terminated. 
				char *p_cust_no			7 bytes. not null-terminated. 
				char *p_stock_no		6 bytes. not null-terminated. 左靠 右補空白 
			Return
				>= 0		OK
				< 0			error 
		*/
		[DllImport("dll_tseCli.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
				public static extern int dll_tseCli_QryStockPos( ref IntPtr p_tseCliHdl, UInt32 reqId, string p_broker_id, string p_cust_no, string p_stock_no );





		///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//
		// 委託&回報 回補 機制 
		//


		/*
			client 回補 開盤至今 某一盤的委託與回報. 

			在 call 這個 fucntion 之前, 請先 set 好 reRpt 相關的 callback. 

			AP call dll_tseCli_recoverRpt( ), 不會擋住 AP 流程. 在底層建立與 server 端的連線後, 隨後 經由 callback 傳回 開盤至今的委託與回報. 
			在回補完之前, 如果發生斷線, 則 DLL 底層自動重新嘗試建立這個回補的連線.
			待回補完之後, DLL 底層自動結束這個回補的連線. 

			Input
				p_tfxCliHdl				經由 dll_tfxCli_init() 與 dll_tfxCli_open() 得到的 Handle.
				pszTargetIP				null-terminated string
				iTargetPort 			回補Server 的 listen port 

				pszUId					null-terminated string. 
										如果 空字串 或 空白字串, 則 Server 傳回所有 User ID 的委託與回報。
										如果填某個 User ID, 則 Server 傳回這個 User ID 的委託與回報。

				MatchRptFlag       		Y: 傳送成回,  N: 不傳送成回
				NewOrdFlag        		Y: 傳送新單委託單,  N: 不傳送新單委託單
				CxlRplOrdFlag       	Y: 傳送刪改單委託單,  N: 不傳送刪改單委託單
				NewOrdOKRptFlag       	Y: 傳送新單委託成功的回報,  N: 不傳送新單委託成功的回報
				NewOrdErrRptFlag       	Y: 傳送新單委託失敗的回報,  N: 不傳送新單委託失敗的回報
				CxlRplOrdOKRptFlag     	Y: 傳送刪改單委託成功的回報,  N: 不傳送刪改單委託成功的回報
				CxlRplOrdErrRptFlag    	Y: 傳送刪改單委託失敗的回報,  N: 不傳送刪改單委託失敗的回報

				stopAfterRecoverFlag 	1: 回補完開盤至今的委託與回報之後, 就 stop.  0: 回補完開盤至今的委託與回報之後, 仍繼續收未來的委託與回報.
		*/
		[DllImport("dll_tseCli.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern int dll_tseCli_recoverRpt( ref IntPtr p_tseCliHdl, string pszTargetIP, int iTargetPort, string pszUId, 
							char MatchRptFlag, char NewOrdFlag, char CxlRplOrdFlag, char NewOrdOKRptFlag, char NewOrdErrRptFlag, 
							char CxlRplOrdOKRptFlag, char CxlRplOrdErrRptFlag, int stopAfterRecoverFlag );  



		[DllImport("dll_tseCli.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern void dll_tseCli_reRpt_set_onSysStat( ref IntPtr p_tseCliHdl, tseCli_reRpt_onSysStatFunc_t ap_reRpt_onSysStat, IntPtr reRpt_onSysStat_ap_param );

		[DllImport("dll_tseCli.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern void dll_tseCli_reRpt_set_onRecvOrderReply( ref IntPtr p_tseCliHdl, tseCli_reRpt_onRecvOrderReplyFunc_t ap_reRpt_onRecvOrderReply, IntPtr reRpt_onRecvOrderReply_ap_param );

		[DllImport("dll_tseCli.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern void dll_tseCli_reRpt_set_onRecvMatchReply( ref IntPtr p_tseCliHdl, tseCli_reRpt_onRecvMatchReplyFunc_t ap_reRpt_onRecvMatchReply, IntPtr reRpt_onRecvMatchReply_ap_param );

		[DllImport("dll_tseCli.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern void dll_tseCli_reRpt_set_onRecvOrder( ref IntPtr p_tseCliHdl, tseCli_reRpt_onRecvOrderFunc_t ap_reRpt_onRecvOrder, IntPtr reRpt_onRecvOrder_ap_param );


		///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		//
		// 工具函數 
		//
		
		public const int tseoms_ErrMsgSize = 128; 
		
		
		[StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Ansi)]
        public struct tseoms_ErrMsg_t
        {		
			[MarshalAs(UnmanagedType.ByValArray, SizeConst = OrderLibConnTronSvr.tseoms_ErrMsgSize)]
			public byte[] ErrMsg;	 
		}
		
		
		/*
			Input
				i32 iRC				Return Code given by the OMS 
				i32 strBufSize		buffer size of strBuf 
				
			Output
				char *strBuf		null-terminated error message string. 
				
			Return 
				strBuf 				null-terminated error message string. 
		*/
		[DllImport("dll_tseCli.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
		public static extern IntPtr dll_tseoms_strerror( UInt32 iRC, IntPtr strBuf, UInt32 strBufSize ); 


		
        #endregion // Import from dll_tseCli.dll




        public OrderLibConnTronSvr(enumType Type)
        {
            _Type = Type;
        }


        #region ConnectToSvr : 連線到 Server
        public bool ConnectToSvr( ref IntPtr ttCliHdl, ref bool IsConnected, string lineID, string basePath, 
                                  string TradeIP, int TradePort, int BkSvrPort, string UId, string Pwd  ) 
        {
            int rc = 0; 

            try
            {
                // 若已經連線則必須先離線
                if (IsConnected)
                {
                    DisconnectFromSvr(ref ttCliHdl);
                }

               
				switch (_Type)
				{
					case enumType.Push:
						// rc = dll_ttCliPush_open(ref ttCliHdl, basePath, lineID, onRtnReply_func, ap_param_onRtnReply);
						/* 
						*  dll_ttCli_open() 的 onRtnReply_func() 與 dll_ttCliPush_open() 的 onRtnReply_func() 可以不同 function. 
						* 
						* */

						Debug.WriteLine("[ConnectToSvr] : <" + ttCliHdl.ToString() + "><" + lineID + "><" + basePath + ">");
						break;
					default:
					
						/*
						 * 
						 *  必須先第一次連線成功之後, DLL 底層才會自動斷線重連.
						 * 
						 */

						rc = dll_tseCli_open( ref ttCliHdl, "61.216.152.219", TradePort, basePath, UId, Pwd, -1 /* sendCoreId */ ); 
						
						// Debug.WriteLine("[ConnectToSvr] : <" + ttCliHdl.ToString() + "><" + lineID + "><" + basePath + ">");
						
						// MessageBox.Show( "dll_tseCli_open, rc=" + rc ); 
						
						if ( rc >= 0 )
						{
							// 回補
							rc = dll_tseCli_recoverRpt( ref ttCliHdl, TradeIP, BkSvrPort, UId, 
								'Y' /* MatchRptFlag */, 'Y' /* char NewOrdFlag */, 'Y' /* CxlRplOrdFlag */, 'Y' /* NewOrdOKRptFlag */, 'Y' /* NewOrdErrRptFlag */, 
								'Y' /* CxlRplOrdOKRptFlag */, 'Y' /* CxlRplOrdErrRptFlag */, 0 /* stopAfterRecoverFlag */ );  
						}
							
		
						break;
				}

				if (rc >= 0) // OK
				{
					IsConnected = true;
					return true;
				}
				else
				{
					IsConnected = false;
					return false;
                } 
            }
            catch (Exception ex)
            {
                Debug.WriteLine("[ConnectToSvr] : " + ex.ToString());
            }

            return false;
        }

        #endregion  // ConnectToSvr : 連線到 Server


        #region DisconnectFromSvr : 切斷連線
        public bool DisconnectFromSvr(ref IntPtr ttCliHdl)
        {
            try
            {
                switch (_Type)
                {
                    case enumType.Push:
                        // dll_ttCliPush_close(ref ttCliHdl);
                        break;
                    default:
						if ( OrderLibGlobal.ConnData.OrderSvr.IsConnected )
						{
							dll_tseCli_close( ref ttCliHdl ); 
							
							OrderLibGlobal.ConnData.OrderSvr.IsConnected = false; 
						}
						
                        break;
                }

                return true;
            }
            catch (Exception ex)
            {
                Debug.WriteLine("[DisconnectFromSvr] : " + ex.ToString());
                return false;
            }
        }
        #endregion //  DisconnectFromSvr : 切斷連線



    } // class ConnTronSvr
}
