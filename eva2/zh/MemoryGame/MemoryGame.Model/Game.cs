using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Timers;

namespace MemoryGame.Model
{
    public class Game
    {
        public class Field
        {
            public int ID { get; private set; }
            public bool Visible { get; set; }

            public Field(int id)
            {
                ID = id;
                Visible = false;
            }
        }

        public class InteractionData
        {
            public int? ClickedID { get; set; } = null;
        }

        private Random random;

        private readonly int size;
        public int Size => size;

        public int TargetFieldID { get; private set; }

        private readonly Field[] fields;
        public IReadOnlyCollection<Field> Fields => fields;

        private readonly InteractionData interactionData;
        public InteractionData Interactions => interactionData;

        public EventHandler TickEvent;
        private Timer tickTimer;

        public Game(int size)
        {
            this.size = size;
            random = new Random();
            fields = CreateShuffled(size);
            TargetFieldID = NewTargetFieldID();
            interactionData = new InteractionData();
            tickTimer = new Timer();
            tickTimer.Interval = 2000;
            tickTimer.Elapsed += (object o, ElapsedEventArgs e) =>
            {
                StepState();
                TickEvent?.Invoke(this, null);
            };
            tickTimer.Start();
        }

        private IEnumerable<Field> InvisibleFields =>
            fields.Where(field => field.Visible == false);

        private int NewTargetFieldID()
        {
            var available = InvisibleFields.ToArray();
            return available[random.Next(0, available.Length)].ID;
        }

        public void ClickID(int id)
        {
            if (interactionData.ClickedID != null) return;

            Field clicked = fields.Where(f => f.ID == id && !f.Visible).FirstOrDefault();
            if (clicked != null)
            {
                interactionData.ClickedID = clicked.ID;
                clicked.Visible = true;
            }
        }

        public void StepState()
        {
            if (TargetFieldID != interactionData.ClickedID && interactionData.ClickedID != null)
            {
                GetField(interactionData.ClickedID ?? -1).Visible = false;
            }
            if (InvisibleFields.ToArray().Length == 0)
            {
                tickTimer.Stop();
                return;
            }
            TargetFieldID = NewTargetFieldID();
            interactionData.ClickedID = null;
        }

        public Field GetField(int id)
        {
            return Fields.Where(field => field.ID == id).FirstOrDefault();
        }

        private Field[] CreateShuffled(int length)
        {
            var arr = Enumerable
                        .Range(0, length)
                        .Select(id => new Field(id))
                        .ToArray();
            for (int i = 0; i < arr.Length; i++)
            {
                int swapIndex = random.Next(i, length);
                var temp = arr[i];
                arr[i] = arr[swapIndex];
                arr[swapIndex] = temp;
            }
            return arr;
        }

    }
}
