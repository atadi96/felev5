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
        private const char START_CHARACTER = 'S';
        private const char FINISH_CHARACTER = 'F';
        private const char ENEMY_CHARACTER = 'E';
        private const char WALL_CHARACTER = 'W';
        private const char FLOOR_CHARACTER = ' ';

        public class MapID
        {
            public readonly string Name;
            public readonly string Filename;
            public readonly int Width;
            public readonly int Height;

            protected MapID(string name, string filename, int width, int height)
            {
                Name = name;
                Filename = filename;
                Width = width;
                Height = height;
            }
        }

        private class MapIDCtor : MapID
        {
            public MapIDCtor(string name, string filename, int width, int height)
                : base(name, filename, width, height) { }
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
                            StreamReader sr = new StreamReader(fs);
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
                    char[] fields = sr.ReadLine().ToCharArray();
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
