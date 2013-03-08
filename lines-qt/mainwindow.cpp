#include "mainwindow.h"
#include <QPainter>
#include <QTime>

#define CELL_SIDE 40

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent)
{
    qsrand(QTime::currentTime().msec());
    pGameField = new Field(W, H);
    pGameField->throwBalls(pGameField, 15);

    pNextBalls = new Field(W, H);
    pGameField->throwBalls(pNextBalls, 3);

    graphicsScene = new QGraphicsScene(0, 0, CELL_SIDE * W, CELL_SIDE * H);
    graphicsView = new GraphicsView(graphicsScene);
    this->setCentralWidget(graphicsView);

    balls.resize(W * H);

    int cell;

    //QGraphicsSvgItem tileBase(":/images/tile.svg");
    for (int x = 0; x < W; x++)
        for (int y = 0; y < H; y++)
        {
            QGraphicsSvgItem *tile = new QGraphicsSvgItem(":/images/tile.svg");
            tile->setPos(x * CELL_SIDE, y * CELL_SIDE);
            tile->setZValue(0);
            //tile->setScale(1.0 / 40.0);
            graphicsScene->addItem(tile);

            if ((cell = pGameField->cell(x, y)))
            {
                QGraphicsSvgItem *ball = new QGraphicsSvgItem(QString(":/images/ball%1.svg").arg(cell));
                ball->setPos(x * CELL_SIDE, y * CELL_SIDE);
                ball->setZValue(3);
                balls[x + W * y] = ball;
                graphicsScene->addItem(ball);
            }
            else if ((cell = pNextBalls->cell(x, y)))
            {
                QGraphicsSvgItem *ball = new QGraphicsSvgItem(QString(":/images/ball%1.svg").arg(cell));
                ball->setScale(0.4);
                ball->setPos((0.3 + x) * CELL_SIDE, (0.3 + y) * CELL_SIDE);
                //ball->setOpacity(0.5);
                ball->setZValue(2);
                balls[x + W * y] = ball;
                graphicsScene->addItem(ball);
            }
        }

    selectItem = new QGraphicsSvgItem(":/images/select.svg");
    selectItem->setZValue(1);
    selectItem->setVisible(false);
    graphicsScene->addItem(selectItem);

    pathMarkers.resize(W * H);
    for (int i = 0; i < W * H; i++)
    {
        QGraphicsSvgItem *item = new QGraphicsSvgItem(":/images/pathmarker.svg");
        item->setZValue(1);
        item->setVisible(false);
        graphicsScene->addItem(item);
    }

    setWindowTitle(tr("Lines-Qt"));
    setMinimumSize(CELL_SIDE * W, CELL_SIDE * H);

    connect(graphicsView, SIGNAL(mouseReleased(QMouseEvent*)), this, SLOT(processMouseEvent(QMouseEvent*)));
}

MainWindow::~MainWindow()
{
    delete pGameField;
    delete pNextBalls;
}


/*void MainWindow::paintEvent(QPaintEvent *)
{
    int n;
    QPainter painter(this);

    int side = qMin(width(), height());
    painter.setRenderHint(QPainter::Antialiasing);
    painter.translate(width() / 2, height() / 2);
    painter.scale(side / (40.0 * pGameField->width()), side / (40.0 * pGameField->height()));

    QColor ballColors[] = {Qt::red, Qt::green, Qt::blue,
                           Qt::yellow, Qt::magenta, Qt::cyan, Qt::white};
    for (int x = 0; x < pGameField->width(); x++)
        for (int y = 0; y < pGameField->height(); y++)
        {
            painter.setBrush(Qt::lightGray);
            painter.drawRect(x * CELL_SIDE - pGameField->width() * CELL_SIDE / 2.0,
                             y * CELL_SIDE - pGameField->height() * CELL_SIDE / 2.0, CELL_SIDE, CELL_SIDE);
            if ((n = pGameField->cell(x, y)) != 0)
            {
                painter.setBrush(ballColors[n - 1]);
                painter.drawEllipse(x * CELL_SIDE + 5 - pGameField->width() * CELL_SIDE / 2.0,
                                    y * CELL_SIDE + 5 - pGameField->height() * CELL_SIDE / 2.0, 30, 30);
            }
        }

    Field search(pGameField);
    pGameField->hasPath(2, 5, 7, 4, &search);

    QPoint pt(7, 4);
    int k;
    while ((k = search.cell(pt.x(), pt.y())) > -1)
    {
        if (pt.x() > 0 && search.cell(pt.x() - 1)))
    }
}*/

void MainWindow::resizeEvent(QResizeEvent *event)
{
    QMainWindow::resizeEvent(event);
    //graphicsView->fitInView(graphicsScene->sceneRect(), Qt::KeepAspectRatio);
}

void MainWindow::processMouseEvent(QMouseEvent *event)
{
    int x = event->x() / CELL_SIDE, y = event->y() / CELL_SIDE;

#ifdef QT_DEBUG
    qDebug("%d, %d", x, y);
#endif

    if (x < 0 || y < 0 || x > W - 1 || y > H - 1)
        return;

    if (pGameField->cell(x, y))
    {
        selection = QPoint(x, y);
        selectItem->setPos(x * CELL_SIDE, y * CELL_SIDE);
        selectItem->setVisible(true);
        return;
    }

    Field pathMap(pGameField);
    if (pGameField->hasPath(selection.x(), selection.y(), x, y, &pathMap))
    {
        Field path(W, H);
        pathMap.shortestPath(x, y, &path);

        for (int x_ = 0; x_ < W; x_++)
            for (int y_ = 0; y_ < W; y_++)
                pathMarkers[x_ + W * y_]->setVisible(path.cell(x_, y_));

        return;

        pGameField->setCell(x, y, pGameField->cell(selection.x(), selection.y()));
        pGameField->setCell(selection.x(), selection.y(), 0);
        QGraphicsSvgItem *ball = balls[selection.x() + W * selection.y()];
        balls[x + W * y] = ball;
        balls[selection.x() + W * selection.y()] = NULL;
        ball->setPos(x * CELL_SIDE,  y * CELL_SIDE);
        selection = QPoint(-1, -1);
        selectItem->setVisible(false);

        //stateMachine.
    }
}
