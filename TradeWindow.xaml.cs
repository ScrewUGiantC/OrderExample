using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;
using OrderExample.Api;
using static OrderExample.Api.OrderLibConnTronSvr;

namespace OrderExample
{
    /// <summary>
    /// Interaction logic for TradeWindow.xaml
    /// </summary>
    public partial class TradeWindow : Window
    {

        OrderLibConnTronSvr.broker_def_t _brokerDef;


        public TradeWindow()
        {
            InitializeComponent();

            _brokerDef = new OrderLibConnTronSvr.broker_def_t()
            {
                ClientIP = B("127.0.0.1", OrderLibConnTronSvr.ClientIP_LEN),
                UDD = B("Test-UDD", OrderLibConnTronSvr.UDD_LEN),
                SALENO = B("Test-UDD", OrderLibConnTronSvr.UDD_LEN),

            };

        }


        private void BuyBtn_OnClick(object sender, RoutedEventArgs e)
        {
            SendNewOrder('1');
        }

        private void SellBtn_OnClick(object sender, RoutedEventArgs e)
        {
            SendNewOrder('2');
        }


        private void SendNewOrder(char chSide)
        {


            OrderLibConnTronSvr.tsecli_order_t tt_order = new OrderLibConnTronSvr.tsecli_order_t(); // Note: 不要用 new, 改從 快速的記憶體管理機制 取得 委託單 memory !!! 

            //// common header of all client messages
            tt_order.mark = OrderLibConnTronSvr.ORDER_MARK;     //  Little Endian

            tt_order.reqId = GetRid();                  // client 填入 電子單編號, Request ID   Little Endian

            // i64 SysUNo;								// 由 library 系統 自動填寫. AP 不用填寫.
            // char SysUId[16];							// 由 library 系統 自動填寫. AP 不用填寫.
            //// 

            tt_order.broker_def = _brokerDef; // 元大 自行定義的 額外欄位. 用於 委託單, 回報單. Yuanta's spec: broker_def is only set in NewOrder, 異動單 不會變更 broker_def.

            ////
            /* 子系統類別 (1-普通股，2-盤後定價，3-零股) PIC X(01)				*/
            tt_order.subsys_name = '1';

            /* 委託功能別 (1-買，2-賣，3-改量，4-取消，5-查詢) PIC X(01)		*/
            tt_order.function_code = chSide;

            /* 券商代號 	PIC X(04)											*/
            tt_order.broker_id = B("9300", 4);

            // tt_order.broker_id = txt_broker_id.Text.ToString().Trim().PadRight(4).ToCharArray();    // 沒有檢查長度是否為 4 


            /* 委託書編號 (委託時為空白，改量、取消時為原委託書編號) PIC X(05)	*/
            tt_order.order_no = B(" ", 5);

            /* 客戶帳號 (含檢碼) 	PIC 9(07)									*/
            tt_order.cust_no = B(AccountTb.Text, 7);

            /* 股票代號 	PIC X(06)											*/
            tt_order.stock_no = B(SymbolTb.Text, 6);

            /*   1:委託種類 (0-現股、普通，1-代辦融資，2-代辦融券，3-自辦融資，4-自辦融券
			 *                           5: 借券 5 (一般借券, 平盤以下不可賣 (只能以比開盤價更高的價格賣出))
										 6: 借券 6 (策略性的特殊借券, 平盤以下可賣. 
										 7: 資 7 (當沖)
										 8: 券 8 (當沖) )
				   ORDER-TYPE          PIC X(01).
	
			   目前 大戶下單 只提供 0-現股、普通，3-自辦融資，4-自辦融券，7: 資 7 (當沖)，8: 券 8 (當沖)
			   改單 刪單 查詢 時, 填空白. 										*/

            tt_order.order_type = '0';   // str_to_one_char(txt_order_type );


            /* 委託單價x100 	PIC 9(04)V9(02)
					只有限價之買賣單填價格 x 100. 譬如 60.65 元, 則填 6065.
	 				指定跌停價, 平盤價, 漲停價時, 填 0. 
					改單 刪單 時, 填 0.											*/

            // tt_order.order_price100 = Convert.ToUInt32(txt_price100);


            var parsed = uint.TryParse(PriceTb.Text.Trim(), out tt_order.order_price100);
            if (!parsed) return;


            /* 委託數量 (普通、盤後定價-張數，零股-股數) PIC 9(04)
					改量時, 整股填欲減少之張數. 
					改量時, 零股填更改後之委託股數. 
					刪單 查詢 時, 填 0.										*/

            // tt_order.order_qty = Convert.ToUInt16(txt_qty);

            parsed = ushort.TryParse(QtyTb.Text.Trim(), out tt_order.order_qty);
            if (!parsed) return;

            if (PriceTypeCb.SelectedIndex == 1)
                tt_order.price_type = '2';  // 限價
            else
            {
                tt_order.price_type = '1';  // 市價

                if (tt_order.order_price100 != 0)
                {
                    return;
                }
            }

            

            if (TifCb.SelectedIndex == 0)  // ROD 
                tt_order.time_in_force = '0';
            else if (TifCb.SelectedIndex == 1)  // IOC
                tt_order.time_in_force = '3';
            else if (TifCb.SelectedIndex == 2)  // FOK 
                tt_order.time_in_force = '4';
            else
            {
                return;
            }
#if DEBUG
            return;
#endif
            OrderLibConnTronSvr.dll_tseCli_send_order(ref OrderLibGlobal.ConnData.OrderSvr.ttCliHdl, ref tt_order);

        }

        public static uint GetRid()
        {
            return (uint)Interlocked.Increment(ref OrderLibGlobal.reqId);
        }

        public static byte[] B(string data, int length)
        {
            var ba = Encoding.GetEncoding("big5").GetBytes(data);
            var r = new byte[length];
            for (var i = 0; i < length; i++)
            {
                if (i < ba.Length)
                    r[i] = ba[i];
                else
                    r[i] = (byte)' ';
            }
            return r;
        }
    }
}
