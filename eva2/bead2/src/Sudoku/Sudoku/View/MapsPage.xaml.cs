using System;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Linq;
using System.Threading.Tasks;

using Xamarin.Forms;
using Xamarin.Forms.Xaml;
using Lopakodo.Persistance;
using Lopakodo.Mechanics;

namespace Lopakodo.View
{
    [XamlCompilation(XamlCompilationOptions.Compile)]
    public partial class MapsPage : ContentPage
    {
        public MapsPage()
        {
            InitializeComponent();
        }

        async void Handle_ItemTapped(object sender, ItemTappedEventArgs e)
        {
            if (e.Item == null)
                return;

            //await DisplayAlert("KONGOU DESS", "Teitokuuu!!!.", "FEUER!!!");
            //MapSelected?.Invoke(this, (IMapID)e.Item);

            //Deselect Item
            ((ListView)sender).SelectedItem = null;
        }
    }
}
