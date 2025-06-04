from textual.app import App, ComposeResult
from textual.widgets import Header, Footer, Button, Static, ListView, ListItem
from textual.containers import Vertical
import subprocess

class DatabaseList(Vertical):
    def compose(self) -> ComposeResult:
        yield Static("Databases:")
        yield ListView(id="db-list")

class RunestoneTUI(App):

    CSS = """
    Screen {
        align: center middle;
    }
    #db-list {
        height: 10;
        width: 50;
        border: round cornflowerblue;
    }
    """

    def compose(self) -> ComposeResult:
        yield Header()
        yield DatabaseList()
        yield Footer()

    def on_mount(self) -> None:
        # Call your list_db.sh script here and parse output
        result = subprocess.run(
            ["bash", "scripts/list_db.sh"],
            capture_output=True,
            text=True
        )
        db_names = []
        for line in result.stdout.splitlines():
            if line.startswith(" "):
                db_names.append(line.strip().split(" ")[0])

        list_view = self.query_one("#db-list", ListView)
        for db in db_names:
            list_view.append(ListItem(Button(db)))

if __name__ == "__main__":
    RunestoneTUI().run()
