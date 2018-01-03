using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Lopakodo.Persistance;
using Lopakodo.Mechanics;

namespace Lopakodo.ViewModel
{
    class ViewMapID
    {
        public string NameText { get; private set; }
        public string SizeText { get; private set; }
        public IMapID MapID { get; private set; }

        public ViewMapID(IMapID mapID)
        {
            NameText = mapID.Name;
            SizeText = string.Format("Size: {0}×{1}", mapID.Width, mapID.Height);
            MapID = mapID;
        }
    }

    class MapsViewModel : ViewModelBase
    {
        public delegate void MapSelectedEvent(object sender, IMapID mapID);
        public MapSelectedEvent MapSelected;

        private ViewMapID _selectedItem;
        public ViewMapID SelectedItem
        {
            get { return _selectedItem; }
            set
            {
                if (_selectedItem != value)
                {
                    _selectedItem = value;
                    OnPropertyChanged("SelectedItem");
                    if (value != null)
                    {
                        MapSelected(this, value.MapID);
                    }
                    SelectedItem = null;
                }
            }
        }
        public ObservableCollection<ViewMapID> Items { get; private set; }

        private IMapRepository<IMapID, FieldType> _mapRepo;

        public MapsViewModel(IMapRepository<IMapID, FieldType> mapRepo)
        {
            SelectedItem = null;
            _mapRepo = mapRepo;
            Items = new ObservableCollection<ViewMapID>(
                _mapRepo.Maps.Select(id => new ViewMapID(id))
            );
        }
    }
}
