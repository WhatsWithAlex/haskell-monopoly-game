# Monopoly board game
### Written in 1000% functional style on Haskell
*With the help of Gloss library*

**Repo structure**:

	.
	├── app                     # App entry point
	├── recource                # Resource images (bmp) files for graphics
	└── src      
        ├── Types.hs            # Types used in app logic
        ├── Const.hs            # All app's modules constants
        ├── Helpers.hs          # Functions to support main logic
        ├── Graphics.hs         # App state drawing functions
        └── Game.hs             # Main app's logic: main logic functions and event handling

**Controls**:
* Space - Throw dices and start turn
* Enter - Pay rent / tax / release from jail
* Y key - Agree to buy property
* N key - Disagree to buy propery (start auction)
* LMB   - Upgrade street or lift property
* RMB   - Sell street upgrade or mortgage property


**Todo List**:
- [X] Define main types and functions
- [X] Define all initial game information and constants (cards, fields, prices)
- [X] Main turn logic (dice throwing, moving) 
- [X] Property logic (buy, rent, upgrade, sell, morgage)
- [ ] Penalties and rewards logic
- [ ] Auction logic
- [X] Other logic (jail, start passing, player bancrupcy etc.)
- [ ] Trade logic (hard)
- [X] Make some UI (board, player figures, player stats)
- [ ] Make good UI (start settings, animations?)
