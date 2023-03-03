using OrderExample.Api;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;

namespace OrderExample
{
    /// <summary>
    /// Interaction logic for LoginWindow.xaml
    /// </summary>
    public partial class LoginWindow : Window
    {
        public LoginWindow()
        {
            InitializeComponent();
        }

        private void ButtonBase_OnClick(object sender, RoutedEventArgs e)
        {
            var strUserId = AccountTb.Text.Trim();
            var strPassword = PasswordTb.Password.Trim();
#if DEBUG
            DialogResult = true;
            return;
#endif
            this.DialogResult = OrderLibGlobal.ConnData.OrderSvr.ConnTronSvr.ConnectToSvr(
                ref OrderLibGlobal.ConnData.OrderSvr.ttCliHdl, ref OrderLibGlobal.ConnData.OrderSvr.IsConnected,
                OrderLibGlobal.ConnData.OrderSvr.lineID, OrderLibGlobal.ConnData.OrderSvr.basePath,
                OrderLibGlobal.ConnData.OrderSvr.TradeIP, OrderLibGlobal.ConnData.OrderSvr.TradePort,
                OrderLibGlobal.ConnData.OrderSvr.BkSvrPort, strUserId, strPassword);
        }
    }
}
