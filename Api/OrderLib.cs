using System;
using System.Collections;
using System.Diagnostics;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

namespace OrderExample.Api
{
    public class OrderLib
    {

        public class LoadIniFile
        {
            [DllImport("kernel32")]
            private static extern int GetPrivateProfileString(string section, string key, string def, StringBuilder retVal, int size, string filePath);
            #region GetString(Section , Key) 讀取 ini , 取得某個 Key 的值
            public static string GetString(string path, string Section, string Key, string DefaultValue)
            {
                string result = "";
                string FileName = Path.GetFullPath(path); 

                StringBuilder Retmsg = new StringBuilder(255);
                int length = GetPrivateProfileString(Section, Key, DefaultValue, Retmsg, 255, FileName);
                result = Retmsg.ToString();

                return result;
            }
            #endregion

            [DllImport("kernel32")]
            private static extern int GetPrivateProfileSectionNames(byte[] lpszReturnBuffer, int nSize, string lpFileName);
            #region GetSectionNames() 讀取 ini , 取得 所有 Section 名稱
            public static ArrayList GetSectionNames(string path)
            {
                ArrayList result = new ArrayList();

                //string FileName = Path.GetFullPath(@".\ini\IOMSTEST.ini");
                //string FileName = Path.GetFullPath(Global.ConnData.jas1BasePath + "ini\\qikOrdSvr.ini");
                string FileName = Path.GetFullPath(path);

                byte[] Retmsg = new byte[32768];
                int Length = GetPrivateProfileSectionNames(Retmsg, 32768, FileName);
                ASCIIEncoding ASCII = new ASCIIEncoding();
                string RetmsgStr = ASCII.GetString(Retmsg).Substring(0, Length);

                string[] RetmsgArr = RetmsgStr.Split((char)0);
                for (int i = 0; i < RetmsgArr.Length; i++)
                {
                    if (RetmsgArr[i].Trim().Length != 0)
                    {
                        result.Add(RetmsgArr[i]);
                    }
                }

                return result;
            }
            #endregion


            #region 由ini檔取得基本連線資訊 LoadInitial_Order()

            /* Purpose: 取得 下單連線的 TradeIP, TradePort, 回補Server 的 BkSvrPort, UId, Pwd 
             *  
             * Input
             *      string basePath
             *      string lineID       連線 ID, e.g. "line001" -- 由 lineID 決定 config file name line001.ini 
             * Output
             *      string TradeIP
             *      int TradePort
			 *		int BkSvrPort		回補Server 的 port number
             * Return
             *      true    成功
             *      false   失敗
             */
            public static bool LoadInitial_Order(string basePath, string lineID, ref string TradeIP, ref int TradePort, ref int BkSvrPort )
            {
                bool result = false;
                string profName = basePath + "\\ini\\" + lineID + ".ini"; 

                try
                {
				// 從 config (e.g. line001.ini) 取得 TradeIP 
				// 
                    TradeIP = GetString(profName, "system" /* section */, "TradeIP" /* key */,  "@@@" /* default value */ ); 
                    if ( TradeIP.CompareTo("@@@") == 0 )
                    {
                        //MessageBox.Show("Configuration Error , check ini file", "Configuration Error", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                        return false;
                    }

				// 從 config (e.g. line001.ini) 取得 TradePort 
				// 
                    string str_TradePort = GetString(profName, "system" /* section */, "TradePort" /* key */, "@@@" /* default value */ );
                    if (str_TradePort.CompareTo("@@@") == 0 )
                    {
                        //MessageBox.Show("Configuration Error , check ini file", "Configuration Error", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                        return false;
                    }

                    try
                    {
                        TradePort = Convert.ToInt32(str_TradePort);
                    }
                    catch( Exception )
                    {
                        return false; 
                    }

				// 從 config (e.g. line001.ini) 取得 BkSvrPort 
				// 
                    string str_BkSvrPort = GetString(profName, "system" /* section */, "BkSvrPort" /* key */, "@@@" /* default value */ );
                    if (str_BkSvrPort.CompareTo("@@@") == 0 )
                    {
                        //MessageBox.Show("Configuration Error , check ini file", "Configuration Error", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                        return false;
                    }

                    try
                    {
                        BkSvrPort = Convert.ToInt32(str_BkSvrPort);
                    }
                    catch( Exception )
                    {
                        return false; 
                    }
					
					
#if false
				// 從 config (e.g. line001.ini) 取得 UId 
				// 
                    UId = GetString(profName, "system" /* section */, "UId" /* key */,  "@@@" /* default value */ ); 
                    if ( UId.CompareTo("@@@") == 0 )
                    {
                        //MessageBox.Show("Configuration Error , check ini file", "Configuration Error", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                        return false;
                    }
					
				// 從 config (e.g. line001.ini) 取得 Pwd 
				// 
                    Pwd = GetString(profName, "system" /* section */, "Pwd" /* key */,  "@@@" /* default value */ ); 
                    if ( Pwd.CompareTo("@@@") == 0 )
                    {
                        //MessageBox.Show("Configuration Error , check ini file", "Configuration Error", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                        return false;
                    }
					
#endif 

                    result = true;
                }
                catch (Exception ex)
                {
                    Debug.WriteLine( "LoadInitial_Order" + ex.ToString());

                    return false;
                }
                return result;
            }
            #endregion // 由ini檔取得基本連線資訊 LoadInitial_Order()




#if false  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

            #region 由ini檔取得基本連線資訊 LoadInitial_Push()

            /* Purpose: 取得 下單連線的 PushIP 與 PushPort
             *  
             * Input
             *      string basePath
             *      string lineID       連線 ID, e.g. "line001" -- 由 lineID 決定 config file name line001.ini 
             * Output
             *      string PushIP
             *      int PushPort
             * Return
             *      true    成功
             *      false   失敗
             */
            public static bool LoadInitial_Push(string basePath, string lineID, ref string PushIP, ref int PushPort)
            {
                bool result = false;
                string profName = basePath + "\\ini\\" + lineID + ".ini";

                try
                {
                    // 從 config (e.g. line001.ini) 取得 PushIP 
                    // 
                    PushIP = GetString(profName, "system" /* section */, "PushIP" /* key */, "@@@" /* default value */ );
                    if (PushIP.CompareTo("@@@") == 0)
                    {
                        //MessageBox.Show("Configuration Error , check ini file", "Configuration Error", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                        return false;
                    }

                    // 從 config (e.g. line001.ini) 取得 PushPort 
                    // 
                    string str_PushPort = GetString(profName, "system" /* section */, "PushPort" /* key */, "@@@" /* default value */ );
                    if (str_PushPort.CompareTo("@@@") == 0)
                    {
                        //MessageBox.Show("Configuration Error , check ini file", "Configuration Error", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                        return false;
                    }

                    try
                    {
                        PushPort = Convert.ToInt32(str_PushPort);
                    }
                    catch(Exception )
                    {
                        return false; 
                    }

                    result = true;
                }
                catch (Exception ex)
                {
                    Debug.WriteLine(ex.ToString());

                    return false;
                }
                return result;
            }
            #endregion  // 由ini檔取得基本連線資訊 LoadInitial_Push()

#endif  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        } // public class LoadIniFile

    } // public class lib
}
