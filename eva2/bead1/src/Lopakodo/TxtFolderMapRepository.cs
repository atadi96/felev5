using System;
using System.Collections.Generic;
using System.IO;
using Lopakodo.Mechanics;
using RipSeiko.Geometry;

namespace Lopakodo
{
    class TxtFolderMapRepository : IMapRepository<TxtFolderMapRepository.MapID, FieldType>
    {
        private const string MAP_EXTENSION = ".map";
        private const char SIZE_SEPARATOR = 'x';
        private const string START_CHARACTER = "S";
        private const string FINISH_CHARACTER = "F";
        private const string ENEMY_CHARACTER = "E";
        private const string WALL_CHARACTER = "W";
        private const string FLOOR_CHARACTER = " ";

        public class MapID
        {
            public readonly string Name;
            public readonly string Filename;

            protected MapID(string name, string filename)
            {
                Name = name;
                Filename = filename;
            }
        }

        private class MapIDCtor : MapID
        {
            public MapIDCtor(string name, string filename)
                : base(name, filename) { }
        }

        private string _sourceDir;

        public TxtFolderMapRepository(string sourceDir)
        {
            _sourceDir = sourceDir;
        }

        public IReadOnlyList<MapID> Maps
        {
            get
            {
                List<MapID> maps = new List<MapID>();
                try
                {
                    string[] mapFiles = Directory.GetFiles(_sourceDir, "*" + MAP_EXTENSION);
                    foreach (string filename in mapFiles)
                    {
                        using (var fs = new FileStream(filename, FileMode.Open))
                        {
                            string mapName = new StreamReader(fs).ReadLine();
                            maps.Add(new MapIDCtor(mapName, filename));
                        }
                    }
                }
                catch (Exception) { }

                return maps;
            }
        }
        //#szemlélet
        public Map<FieldType> LoadMap(MapID mapID)
        {
            using (var fs = new FileStream(mapID.Filename, FileMode.Open))
            {
                StreamReader sr = new StreamReader(fs);
                string mapName = sr.ReadLine();
                string[] sizeData = sr.ReadLine().Split(SIZE_SEPARATOR);
                int width = Int32.Parse(sizeData[0]);
                int height = Int32.Parse(sizeData[1]);
                FieldType[,] fieldData = new FieldType[height, width];
                Point startingPoint = Point.Zero;
                Point finishPoint = Point.Zero;
                List<Point> enemyStarters = new List<Point>();

                for (int y = 0; y < height; y++)
                {
                    string[] fields = sr.ReadLine().Split();
                    for (int x = 0; x < width; x++)
                    {
                        switch (fields[x])
                        {
                            case START_CHARACTER:
                                startingPoint = new Point(x, y);
                                fieldData[x, y] = FieldType.Ground;
                                break;
                            case FINISH_CHARACTER:
                                finishPoint = new Point(x, y);
                                fieldData[x, y] = FieldType.Ground;
                                break;
                            case ENEMY_CHARACTER:
                                enemyStarters.Add(new Point(x, y));
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
