using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Xamarin.Forms;

namespace Lopakodo.View
{
    class WidthBoxView : BoxView
    {
        protected override void OnSizeAllocated(double width, double height)
        {
            base.OnSizeAllocated(width, width);
        }
    }
}
