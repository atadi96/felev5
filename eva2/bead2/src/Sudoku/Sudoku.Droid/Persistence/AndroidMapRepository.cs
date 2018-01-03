using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using Android.App;
using Android.Content;
using Android.OS;
using Android.Runtime;
using Android.Views;
using Android.Widget;
using System.IO;
using Xamarin.Forms;
using Lopakodo.Persistance;
using Lopakodo.Droid.Persistance;
using Lopakodo.Mechanics;
using Geometry = RipSeiko.Geometry;

[assembly: Dependency(typeof(AndroidMapRepository))]
namespace Lopakodo.Droid.Persistance
{
    public class AndroidMapRepository : BaseMapRepository
    {
        private const string MAP_EXTENSION = ".map";
        private const char SIZE_SEPARATOR = 'x';
        private const char START_CHARACTER = 'S';
        private const char FINISH_CHARACTER = 'F';
        private const char ENEMY_CHARACTER = 'E';
        private const char WALL_CHARACTER = 'W';
        private const char FLOOR_CHARACTER = ' ';

        private class MapIDCtor : IMapID
        {
            public string Filename { get; private set; }

            public int Height { get; private set; }

            public string Name { get; private set; }

            public int Width { get; private set; }

            public MapIDCtor(string name, string filename, int width, int height)
            {
                Name = name;
                Height = height;
                Filename = filename;
                Width = width;
            }
        }

        public override IReadOnlyList<IMapID> Maps
        {
            get
            {
                List<IMapID> maps = new List<IMapID>();
                try
                {
                    string[] mapFiles =
                        new string[] { "bermuda", "midway", "test" }
                            .Select(name => string.Format("{0}{1}", name, MAP_EXTENSION))
                            .ToArray();
                    foreach (string filename in mapFiles)
                    {
                        Stream stream = GetFile(filename);
                        using (StreamReader sr = new StreamReader(stream))
                        {
                            //StreamReader sr = new StreamReader(fs);
                            string mapName = sr.ReadLine();
                            string[] sizeData = sr.ReadLine().Split(SIZE_SEPARATOR);
                            int width = Int32.Parse(sizeData[0]);
                            int height = Int32.Parse(sizeData[1]);
                            maps.Add(new MapIDCtor(mapName, filename, width, height));
                        }
                    }
                }
                catch (Exception e) { }

                return maps;
            }
        }

        public override Map<FieldType> LoadMap(IMapID mapID)
        {
            using (var fs = GetFile(mapID.Filename))
            {
                StreamReader sr = new StreamReader(fs);
                string mapName = sr.ReadLine();
                string[] sizeData = sr.ReadLine().Split(SIZE_SEPARATOR);
                int width = Int32.Parse(sizeData[0]);
                int height = Int32.Parse(sizeData[1]);
                FieldType[,] fieldData = new FieldType[height, width];
                Geometry.Point startingPoint = Geometry.Point.Zero;
                Geometry.Point finishPoint = Geometry.Point.Zero;
                List<Geometry.Point> enemyStarters = new List<Geometry.Point>();

                for (int y = 0; y < height; y++)
                {
                    char[] fields = sr.ReadLine().ToCharArray();
                    for (int x = 0; x < width; x++)
                    {
                        switch (fields[x])
                        {
                            case START_CHARACTER:
                                startingPoint = new Geometry.Point(x, y);
                                fieldData[x, y] = FieldType.Ground;
                                break;
                            case FINISH_CHARACTER:
                                finishPoint = new Geometry.Point(x, y);
                                fieldData[x, y] = FieldType.Ground;
                                break;
                            case ENEMY_CHARACTER:
                                enemyStarters.Add(new Geometry.Point(x, y));
                                fieldData[x, y] = FieldType.Ground;
                                break;
                            case WALL_CHARACTER:
                                fieldData[x, y] = FieldType.Wall;
                                break;
                            case FLOOR_CHARACTER:
                            default:
                                fieldData[x, y] = FieldType.Ground;
                                break;
                        }
                    }
                }
                return new Map<FieldType>(
                    fieldData,
                    startingPoint,
                    finishPoint,
                    enemyStarters.ToArray()
                );
            }
        }
    }
}