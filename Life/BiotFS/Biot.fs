namespace BamaLlama.Alife
open System
module Biot = 
   let MAX_LIMBS = 24
   let MAX_LINES = 256
   let BASIC_LINES = 256
   let MAX_ENERGY_HISTORY = 1024
    enum {
		IS_HUNGRY = 0,  // Histeresis
		IS_INJURED,     // Any injury
		IS_LOW_ENERGY,	// Has less than genetic energy
		IS_FERTILE,		// Is fertile
		IS_MALE,		// Is a male
		IS_ASEXUAL,		// Is asexual
		IS_OLD,			// upper genetic
		IS_YOUNG,		// lower genetic
		IS_ADULT,		// Full grown
	};

	enum {
		STATE_BIOT_STARVING   = 0x00000100,
		STATE_BIOT_HUNGRY     = 0x00000200,
		STATE_BIOT_SICK       = 0x00000400,
		STATE_BIOT_DAMAGED    = 0x00000800,
		STATE_BIOT_MALE       = 0x00001000,
		STATE_BIOT_TOUCH      = 0x00002000, //??
		STATE_BIOT_ADULT      = 0x00004000,
		STATE_BIOT_OLD        = 0x00008000,
		STATE_LIMB_DAMAGED    = 0x00010000,
		STATE_LIMB_SEVERED    = 0x00020000,
		STATE_LIMB_HAVE_EATEN = 0x00040000,
		STATE_LIMB_BEEN_EATEN = 0x00080000,
		STATE_LIMB_ATTACKED   = 0x00100000,
		STATE_LIMB_SEE_FOOD   = 0x00200000,
		STATE_LIMB_SEE_MATE   = 0x00400000,
		STATE_LIMB_SEE_ATTACKER = 0x00800000,
		STATE_LIMB_SEE_DEFENDER = 0x02000000,
		STATE_LIMB_SEE_LIGHT    = 0x04000000,
		STATE_LIMB_SEE_FRIEND   = 0x08000000,
		STATE_LIMB_SEE_MALE     = 0x10000000,
		STATE_LIMB_SEE_PARENT   = 0x20000000,
		STATE_LIMB_SEE_WALL     = 0x40000000,
		STATE_LIMB_SEE_CHILD    = 0x80000000
	};

    enum {
		PERCENT_AGE,
		PERCENT_ENERGY,
	};

    enum {
		NORMAL,
		REDRAW,
		REFORM,
		RECALCULATE,
		GROW
	};


    type Motion = 
        {
            period : byte;
            nPeno : byte;
            frequency : byte

            dx : double;
            dy : double;
            dr : double;
        }

    let MAX_COLLISIONS = 5

    type Redraw() =
       let mutable redraw = false; 
       member x.ShouldRedraw = redraw
       member x.SetRedraw(bRedraw) = if bRedraw then redraw <- true
       member x.ClearRedraw() = redraw <- false;

    type CollisionPoint = 
        {
            envTime : UInt64
            nLine : int
            eLine : int
            hits : int
            dx : double
            dy : double
            dr : double
            deltaLength : int
            deltaEnergy : int
            interact : bool
            collide : bool
        }
    with 
        static member NewCollisionPoint(tm, nl, el, hts,dx,dy,dr,dl,de,interact, collide) = 
           {
              envTime = tm; nLine = nl; eLine = el; hits = hts; 
              dx = dx; dy = dy; dr = dr; deltaLength = dl; 
              deltaEnergy = de; interact = interact; collide = collide 
           }
     
    type biot(name, worldName, fatherName,  fatherWorldName) =
        let mutable id = 0UL
        let mutable fatherId = 0UL
        let mutable motherId = 0UL
        let mutable mateId = 0UL
        let mutable generation = 0UL
        let mutable fatherGeneration = 0UL
        let mutable bmpWidth = 0
        let mutable bmpHeight = 0
        let mutable newType = 0
        let mutable age = 0
        let mutable maxage = 0
        let mutable die = false;
        let mutable energy = 0
        let mutable childBaseEnergy = 0
        let mutable collisions =  null//(None : CollisionSummary option)
        let mutable vector = null
        let angles = Array.create(MAX_LINES, 0)
        let anglesDrawn = Array.create(MAX_LINES, 0)
        let retractDrawn = Array.create(MAX_LIMBS, 0) 
        let retractRadius = Array.create(MAX_LIMBS, 0) 
        let retractSegment = Array.create(MAX_LIMBS, 0) 
        let limbCommands = Array.create(MAX_LIMBS, newLimbCommandStore())
        let mutable commandArray = null
        let mutable commandArray2 = null
        let mutable internalState = 0
        let mutable terminateEvent = false
        let mutable sick = 0
        let mutable drawn = false
        let mutable selected = false
        let redraw = new Redraw()
        let startPoint = Array.create(MAX_LINES, 0)
        let stopPoint = Array.create(MAX_LINES, 0)
        let distance =  Array.create(MAX_LINES, 0)
        let ``trait`` = new GeneTrait()
        let env = null
        let statEnergy = Array.create(MAX_ENERGY_HISTORY, 0.0)
        let mutable statIndex = 0
        let angleRateLimit = 0
        let staticsInitialized = false
        let bitmap = null
        let mutable bonusRatio = 0.0
        let mutable livingChildren = 0
        let mutable totalChildren = 0
        let origin = new BPoint()
        let lineTypes = Array.create(MAX_LINES, 0)
        let lineStates = Array.create(MAX_LINES, 0)
        let color = Array.create(MAX_LINES, 0uy)
        let mutable injured = false
        let mutable genes2 = 0
        let mutable topY = 0
        let mutqable bottomY = 0;
        let mutble leftX = 0
        let mutqable rightX = 0
        let mutable ratio = 0
        let mutable lastType = 0
        let mutable lastLeft = 0
        let mutable laastTop = 0
        let mutable max_genes = 0
        let mutable genes = 0
        let mutable turnBenefit = 0UL
        let mutable totalDistance = 0UL
        let colorDistance = Array.create(BASIC_LINES, 0UL)
        let mutable adultBaseEnergy = 0UL
        let mutable stepEnergy = 0UL
        let mutable massCenter = new BPoint()

    type CollisionObject() = 
       let envTime = 0UL
       let hits = 0
       let biot = (None : biot option)