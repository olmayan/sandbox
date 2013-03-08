#include <QApplication>
#include "mainwindow.h"
int main(int argc, char *argv[])
{
    Q_INIT_RESOURCE(lines_qt);

    QApplication app(argc, argv);

    MainWindow *window = new MainWindow();
    window->show();

    return app.exec();
}
