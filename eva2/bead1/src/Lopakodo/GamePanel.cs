using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using Lopakodo.Mechanics;
using Geometry = RipSeiko.Geometry;

namespace Lopakodo
{
    partial class GamePanel : UserControl
    {
        private GameState gameState;
        private EventHandler quitEvent;

        private GameState.Direction selectedDirection = GameState.Direction.None;

        public GamePanel(Map<FieldType> selectedMap, EventHandler quitEvent)
        {
            InitializeComponent();
            this.gameState = new GameState(selectedMap);
            this.quitEvent = quitEvent;
            SetStyle(ControlStyles.ResizeRedraw, true);
            drawPanel.Resize += (object o, EventArgs a) => { drawPanel.Invalidate(); };
            KeyDown +=
                (object o, KeyEventArgs ke) =>
                {
                    if (ke.KeyData == Keys.Escape)
                    {
                        stepTimer.Enabled = !stepTimer.Enabled;
                    }
                    if (!stepTimer.Enabled)
                    {
                        return;
                    }
                    switch (ke.KeyData)
                    {
                        case Keys.W:
                            selectedDirection = GameState.Direction.Up;
                            break;
                        case Keys.D:
                            selectedDirection = GameState.Direction.Right;
                            break;
                        case Keys.S:
                            selectedDirection = GameState.Direction.Down;
                            break;
                        case Keys.A:
                            selectedDirection = GameState.Direction.Left;
                            break;
                        default:
                            break;
                    }
                };
            stepTimer.Tick += (object o, EventArgs e) => { UpdateGame(); };
            stepTimer.Enabled = true;
        }

        private void UpdateGame()
        {
            gameState.StepState(selectedDirection);
            selectedDirection = GameState.Direction.None;
            drawPanel.Invalidate();
            if (gameState.Status != GameState.GameStatus.OnGoing)
            {
                stepTimer.Enabled = false;
                MessageBox.Show(gameState.Status == GameState.GameStatus.Won ? "Mission Success!" : "Mission Failed!", "End Game");
                quitEvent?.Invoke(this, null);
            }
        }

        private void drawField(Graphics graphics, FieldType type, System.Drawing.Rectangle target)
        {
            Func<FieldType, Color> fieldColor =
                (FieldType fieldType) =>
                {
                    switch (fieldType)
                    {
                        case FieldType.Ground:
                            return Color.DarkBlue;
                        case FieldType.Wall:
                            return Color.Green;
                        default:
                            return Color.Black;
                    }
                };
            Brush brush = new SolidBrush(fieldColor.Invoke(type));
            graphics.FillRectangle(brush, target);
        }

        private void drawPanel_Paint(object sender, PaintEventArgs e)
        {
            Func<Geometry.Size, float> getScale =
                (mapSize_) =>
                {
                    float mapScale = (float)mapSize_.X / mapSize_.Y;
                    float panelScale = (float)drawPanel.Size.Width / drawPanel.Size.Height;
                    if (panelScale >= mapScale) //a panel szélesebb mint a kép => a magasság lesz szűkebb
                    {
                        return (float)drawPanel.Size.Height / mapSize_.Y;
                    }
                    else
                    {
                        return (float)drawPanel.Size.Width / mapSize_.X;
                    }
                };
            Geometry.Size mapSize = gameState.Map.Size;
            float scale = getScale(mapSize);
            var translation =
                new Geometry.Vector2F(
                    (drawPanel.Size.Width - mapSize.X * scale) / 2,
                    (drawPanel.Size.Height - mapSize.Y * scale) / 2
                );
            Geometry.Matrix3F toCenter = Geometry.Matrix3F.Translate(translation);
            Geometry.Matrix3F camera = toCenter * Geometry.Matrix3F.Scale(scale);
            Func<Geometry.Point, Rectangle> transform =
                (Geometry.Point p) =>
                {
                    var v = (Geometry.Point)(Geometry.PointF)(camera * new Geometry.Vector2F(p.X, p.Y));
                    int size = (int)scale + 1;
                    return new Rectangle(v.X, v.Y, size, size);
                };

            Graphics gr = e.Graphics;
            gr.Clear(Color.Black);
            for (int x = 0; x < mapSize.X; x++)
            {
                for (int y = 0; y < mapSize.Y; y++)
                {
                    FieldType? f = gameState.Map[x, y];
                    Rectangle rect = transform(new Geometry.Point(x, y));
                    drawField(gr, f.Value, rect);
                }
            }
            Brush winBrush = new SolidBrush(Color.LightGreen);
            gr.FillRectangle(winBrush, transform(gameState.Map.FinishPosition));
            foreach (var field in gameState.VisibleFields)
            {
                gr.DrawImage(Pictures.Radar, transform(field));
            }
            foreach (var enemy in gameState.Enemies)
            {
                gr.DrawImage(Pictures.Rotate(Pictures.Destroyer, GameState.FromVector2(enemy.Direction)), transform(enemy.Position));
            }
            gr.DrawImage(
                Pictures.Rotate(
                    Pictures.Submarine,
                    GameState.FromVector2(gameState.Player.Direction)
                ),
                transform(gameState.Player.Position)
            );
        }
    }
}
