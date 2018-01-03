using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using Xamarin.Forms;
using Xamarin.Forms.Xaml;

namespace Lopakodo.View
{
    [XamlCompilation(XamlCompilationOptions.Compile)]
    public partial class GameView : ContentPage
    {
        public GameView()
        {
            InitializeComponent();
            MapFlowList.SizeChanged += MapFlowList_SizeChanged;
            BindingContextChanged += GameView_BindingContextChanged;
        }

        private void GameView_BindingContextChanged(object sender, EventArgs e)
        {
            if (BindingContext != null)
            {
                var gameModel = (ViewModel.GameViewModel)BindingContext;
                gameModel.PropertyChanged += (object o, System.ComponentModel.PropertyChangedEventArgs propCh) =>
                {
                    if (propCh.PropertyName == "MapWidth")
                    {
                        MapFlowList.RowHeight = (int)(MapFlowList.Width / gameModel.MapWidth);
                    }
                };
            }
        }

        private void MapFlowList_SizeChanged(object sender, EventArgs e)
        {
            if (BindingContext != null)
            {
                var gameModel = (ViewModel.GameViewModel)BindingContext;
                MapFlowList.RowHeight = (int)(MapFlowList.Width / gameModel.MapWidth);
            }
        }
    }
}