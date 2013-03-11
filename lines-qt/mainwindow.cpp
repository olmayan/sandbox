#include "mainwindow.h"
#include <QPainter>
#include <QTime>
#include <QPropertyAnimation>

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
    for (int x = 0; x < W; x++)
        for (int y = 0; y < H; y++)
        {
            QGraphicsSvgItem *item = new QGraphicsSvgItem(":/images/pathmarker.svg");
            item->setPos(x * CELL_SIDE, y * CELL_SIDE);
            item->setZValue(1);
            item->setVisible(false);
            graphicsScene->addItem(item);
            pathMarkers[x + W * y] = item;
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
        QGraphicsSvgItem *ball = balls[selection.x() + W * selection.y()];

        selectItem->setVisible(false);

        //for (int x_ = 0; x_ < W; x_++)
        //    for (int y_ = 0; y_ < H; y_++)
        //        pathMarkers[x_ + W * y_]->setVisible(path.cell(x_, y_));

        //for (int i = 0; i < W * H; i++)
        //    pathMarkers[i]->setVisible(false);

        QLinkedList<QPoint> waypoints = pathMap.waypoints(x, y);
        QLinkedListIterator<QPoint> iter(waypoints);
#ifdef QT_DEBUG
        qDebug("%d", waypoints.count());
#endif

        group = new QSequentialAnimationGroup(graphicsScene);

        QPoint cur = iter.next();
        while (iter.hasNext())
        {
            QPoint next = iter.next();
            int diff = qAbs(next.x() - cur.x()) + qAbs(next.y() - cur.y());
            QPropertyAnimation *anim = new QPropertyAnimation(ball, "pos");
            anim->setDuration(diff * 50);
            anim->setStartValue(QPoint(cur.x() * CELL_SIDE, cur.y() * CELL_SIDE));
            anim->setEndValue(QPoint(next.x() * CELL_SIDE, next.y() * CELL_SIDE));
            group->addAnimation(anim);

            //pathMarkers[next.x() + W * next.y()]->setVisible(true);

            cur = next;
        }

        pGameField->setCell(x, y, pGameField->cell(selection.x(), selection.y()));
        pGameField->setCell(selection.x(), selection.y(), 0);

        balls[x + W * y] = balls[selection.x() + W * selection.y()];
        balls[selection.x() + W * selection.y()] = NULL;

        selection = QPoint(-1, -1);

        group->start(QAbstractAnimation::DeleteWhenStopped);
    }
}
