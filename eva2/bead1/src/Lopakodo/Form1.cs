using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace Lopakodo
{
    public class LopakodoViewEventArgs : EventArgs
    {

    }

    public delegate void LopakodoViewEvent(object sender, LopakodoViewEventArgs args);

    public interface ILopakodoViewState
    {
        LopakodoViewEvent Result { get; set; }
    }

    public partial class Form1 : Form
    {
        Stack<ILopakodoViewState> controls = new Stack<ILopakodoViewState>(5);

        public Form1()
        {
            InitializeComponent();
        }
    }
}
