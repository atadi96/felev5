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
    public partial class Form1 : Form
    {
        private const string MapFolder = "./Maps";

        private class MapSelectionEventArgs : EventArgs
        {
            public readonly TxtFolderMapRepository.MapID SelectedMap;

            public MapSelectionEventArgs(TxtFolderMapRepository.MapID selected)
            {
                SelectedMap = selected;
            }
        }

        private Control MenuControl;
        private Control SelectMapControl;
        private Control LoadingControl;
        private Control GameControl;

        private AnchorStyles AllAnchors =
            AnchorStyles.Bottom |
            AnchorStyles.Left |
            AnchorStyles.Right |
            AnchorStyles.Top;

        private TxtFolderMapRepository MapRepo = new TxtFolderMapRepository(MapFolder);

        public Form1()
        {
            InitializeComponent();
            MenuControl = CreateMenuControl(
                NewGameButton_Click,
                (object o, EventArgs e) => Close()
            );
            Controls.Add(MenuControl);
            Highlight(MenuControl);
        }

        private void NewGameButton_Click(object sender, EventArgs e)
        {
            SetLoading(true);
            IReadOnlyCollection<TxtFolderMapRepository.MapID> maps;
            maps = MapRepo.Maps;
            SetLoading(false);
            SelectMapControl = CreateSelectMapControl(
                maps,
                (object o, MapSelectionEventArgs m) => {
                    Highlight(MenuControl);
                    Controls.Remove(SelectMapControl);
                    SelectMapControl = null;
                    if (m.SelectedMap != null)
                    {
                        var map = MapRepo.LoadMap(m.SelectedMap);
                        GameControl = new GamePanel(
                            map,
                            (object o2, EventArgs e2) =>
                            {
                                Highlight(MenuControl, GameControl);
                                Controls.Remove(GameControl);
                                GameControl = null;
                            }
                        );
                        GameControl.Dock = DockStyle.Fill;
                        Controls.Add(GameControl);
                        Highlight(GameControl, MenuControl);
                    }
                }
            );
            Controls.Add(SelectMapControl);
            Highlight(SelectMapControl, MenuControl);

        }

        private void Highlight(Control control, Control current = null)
        {
            control.BringToFront();
            current?.SendToBack();
            control.Focus();
        }

        private void SetFill(Panel panel)
        {
            panel.AutoSize = true;
            panel.AutoSizeMode = AutoSizeMode.GrowAndShrink;
            panel.Dock = DockStyle.Fill;
        }

        private void SetFill(Button panel)
        {
            panel.AutoSize = true;
            panel.AutoSizeMode = AutoSizeMode.GrowOnly;
            panel.Dock = DockStyle.Fill;
        }

        private Control CreateMenuControl(EventHandler newGame, EventHandler exit)
        {
            TableLayoutPanel panel = new TableLayoutPanel();
            SetFill(panel);
            panel.ColumnStyles.Clear();
            panel.RowStyles.Clear();
            panel.ColumnCount = 3;
            panel.RowCount = 5;
            panel.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 33));
            panel.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 34));
            panel.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 33));
            for (int i = 0; i < panel.RowCount; i++)
            {
                panel.RowStyles.Add(new RowStyle(SizeType.Percent, 20));
            }

            Button startButton = new Button();
            startButton.Text = "New Game";
            SetFill(startButton);
            startButton.Click += newGame;
            Button exitButton = new Button();
            exitButton.Text = "Exit";
            SetFill(exitButton);
            exitButton.Click += exit;

            panel.Controls.Add(startButton, 1, 1);
            panel.Controls.Add(exitButton, 1, 3);
            return panel;
        }

        private Control SingleMapControl(TxtFolderMapRepository.MapID map, EventHandler<MapSelectionEventArgs> handler)
        {
            GroupBox box = new GroupBox();
            box.Text = map.Name;
            box.Height = 100;

            Label sizeLabel = new Label();
            sizeLabel.Top = 40;
            sizeLabel.Text = "Size: " + map.Width + "x" + map.Height;
            sizeLabel.Anchor = AnchorStyles.Left;
            sizeLabel.Dock = DockStyle.Fill;

            Button loadButton = new Button();
            loadButton.Top = 40;
            loadButton.Text = "Start";
            loadButton.Anchor = AnchorStyles.Right;
            loadButton.Dock = DockStyle.Right;
            var ok = loadButton.Margin;
            ok.Right = 0;
            loadButton.Margin = ok;
            loadButton.Click += (object o, EventArgs e) =>
                handler(o, new MapSelectionEventArgs(map));

            box.Controls.Add(sizeLabel);
            box.Controls.Add(loadButton);
            box.Anchor = AnchorStyles.Left | AnchorStyles.Right | AnchorStyles.Top;
            box.Margin = new Padding(0, 0, 0, 12);
            return box;
        }

        private Control CreateSelectMapControl(
            IReadOnlyCollection<TxtFolderMapRepository.MapID> maps,
            EventHandler<MapSelectionEventArgs> mapSelectionEventHandler
        ) {
            var mapControls =
                maps.Select(map => SingleMapControl(map, mapSelectionEventHandler))
                    .ToArray();
            Panel flPanel = new Panel();
            flPanel.AutoScroll = true;
            SetFill(flPanel);
            for (int i = 0; i < mapControls.Length; i++)
            {
                mapControls[i].Top = i * 100;
                flPanel.Controls.Add(mapControls[i]);
            }
            if (mapControls.Length == 0)
            {
                Label label = new Label();
                label.Height = 100;
                label.Text = "No maps available";
                flPanel.Controls.Add(label);
            }
            Panel backPanel = new Panel();
            backPanel.Anchor = AnchorStyles.Top | AnchorStyles.Right;
            backPanel.Top = Math.Max(mapControls.Length, 1) * 100;
            backPanel.Height = 50;
            Button backButton = new Button();
            backButton.Text = "Back";
            backButton.Dock = DockStyle.Right;
            backButton.Click += (object o, EventArgs e) =>
                mapSelectionEventHandler(o, new MapSelectionEventArgs(null));
            backPanel.Controls.Add(backButton);
            flPanel.Controls.Add(backPanel);
            flPanel.Padding = new Padding(12);
            return flPanel;
        }

        private void SetLoading(bool isLoading)
        {
            return;
            if (isLoading)
            {
                Highlight(LoadingControl);
            }
            else
            {
                LoadingControl.SendToBack();
            }
        }
    }
}
